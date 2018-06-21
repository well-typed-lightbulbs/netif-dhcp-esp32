open Result
open Lwt.Infix
open Mirage_net

let src = Logs.Src.create "netif" ~doc:"Mirage esp32 network module"
module Log = (val Logs.src_log src : Logs.LOG)

module type S = Mirage_net_lwt.S

module Make(Time : Mirage_time_lwt.S) (Netif : Mirage_net_lwt.S) = struct

type +'a io = 'a Netif.io

type status = Disconnected | Look_for_IP | IP_obtained

type t = {
    netif: Netif.t; 
    mutable dhcp_client: Dhcp_client.t; 
    push: Mirage_protocols_lwt.ipv4_config option -> unit;
    mutable status: status;
    got_ip: unit Lwt_condition.t;
} 

let sleep_interval = Duration.of_sec 4 

type error = Netif.error

let pp_error = Netif.pp_error


let rec get_lease t =
  Log.info (fun f -> f "DHCPDISCOVER SENT");
  t.status <- Look_for_IP;
  let (client, dhcpdiscover) = Dhcp_client.create (Netif.mac t.netif) in
  t.dhcp_client <- client;
  Netif.write t.netif dhcpdiscover >>= function
    | Error e -> Lwt.return_false
    | Ok () -> 
    Time.sleep_ns sleep_interval >>= fun () ->
    match Dhcp_client.lease t.dhcp_client with 
    | Some lease -> Lwt.return_true
    | None -> get_lease t

let rec do_renew t = 
  t.status <- Look_for_IP;
  (* Put client in a renewing state *)
  match Dhcp_client.renew t.dhcp_client with 
    | `Noop -> Lwt.return_false
    | `Response (c, buf) ->
  (* Update client state *)
  t.dhcp_client <- c;
  (* Write request *)
  Netif.write t.netif buf 
  >>= fun _ ->
  (* Wait for IP retrieval, and retry on time out *)
  Lwt.pick [
    (Lwt_condition.wait t.got_ip >>= fun _ -> Lwt.return_true);
    (Time.sleep_ns (Duration.of_sec 4) >>= fun _ -> Lwt.return_false);
  ]
  >>= function
  | true -> Lwt.return_true
  | false -> (do_renew[@tailcall]) t
  
let wait_for_disconnection () =
    OS.Event.wait_for_event (Wifi.id_of_event Wifi.STA_disconnected) >>= fun _ ->
    Lwt.return_false

(* Lease renewal loop *)
let rec lease_renewal t = 
    Log.info (fun f -> f "DHCP: Got a lease");
    let lease = match Dhcp_client.lease t.dhcp_client with
      | Some lease -> lease
      | None -> assert false 
    in
    let t1 = match Dhcp_wire.find_renewal_t1 lease.options with 
      | Some t1 -> Int32.to_int t1 
      | None -> 1800
    in
    let t2 = match Dhcp_wire.find_rebinding_t2 lease.options with 
      | Some t2 -> Int32.to_int t2
      | None -> 2*t1 - t1/2
    in
    let expires = match Dhcp_wire.find_ip_lease_time lease.options with 
      | Some expires -> Int32.to_int expires
      | None -> 2*t1
    in
    (* Wait for lease renewal timeout, do_renew will return true when a new lease is obtained. *)
    Lwt.pick [
      (Time.sleep_ns (Duration.of_sec t2) >>= fun _ -> Lwt.return_false); 
      (Time.sleep_ns (Duration.of_sec t1) >>= fun _ -> do_renew t)
    ] 
    >>= function
    | true -> (lease_renewal[@tailcall]) t
    | false -> Lwt.return_unit
    >>= fun _ ->
    (* No lease renewal could happen, try a new DHCPDISCOVER *)
    Lwt.pick [
      (Time.sleep_ns (Duration.of_sec (expires - t2)) >>= fun _ -> Lwt.return_false); 
      (get_lease t)
      ]
    >>= function
    | true -> (lease_renewal[@tailcall]) t
    | false -> Lwt.return_unit
    

(* Listen for wifi events to update dhcp lookup status *)
let rec status_updater t =
      Log.info (fun f -> f "DHCP: Waiting for connection");
      OS.Event.wait_for_event (Wifi.id_of_event Wifi.STA_connected) 
    >>= fun _ ->
      Log.info (fun f -> f "DHCP: Looking for IP");
      t.status <- Look_for_IP;
      Lwt.pick [get_lease t; wait_for_disconnection ()] 
    >>= function 
      | true -> lease_renewal t
      | false -> Lwt.return_unit 
    >>= fun _ ->
      Log.info (fun f -> f "DHCP: Disconnected");
      t.status <- Disconnected;
      status_updater t

(* Start a background process to react on wifi events and lease timeout. *)
let connect _time netif =
  let (client, dhcpdiscover) = Dhcp_client.create (Netif.mac netif) in
  let (dhcp, push) = Lwt_stream.create () in
  let got_ip = Lwt_condition.create () in
  let t = {
      got_ip;
      netif;
      dhcp_client=client;
      push;
      status=Disconnected
  } 
  in
  Lwt.ignore_result (status_updater t);
  Lwt.return (t, dhcp)

let disconnect _t =
  Lwt.return_unit

type macaddr = Macaddr.t
type page_aligned_buffer = Io_page.t
type buffer = Cstruct.t

(* Use packet for DHCP and retransmit to IP stack if the packet is useless. *)
let dhcp_packet t buf = 
  match Dhcp_client.input t.dhcp_client buf with 
  | `Noop -> Lwt.return_true
  | `Response (s, action) -> begin 
    Netif.write t.netif action >>= function
    | Error e -> Lwt.return_false
    | Ok () -> t.dhcp_client <- s; Lwt.return_false
  end
  | `New_lease (status, lease) ->
    let open Dhcp_wire in
    let address = lease.yiaddr in
    begin
    match Dhcp_wire.find_subnet_mask lease.options with 
    | None -> ()
    | Some subnet -> 
      let network = Ipaddr.V4.Prefix.of_netmask subnet address in
      let valid_routers = Dhcp_wire.collect_routers lease.options in 
      t.status <- IP_obtained;
      Lwt_condition.broadcast t.got_ip ();
      begin
        match valid_routers with 
        | [] -> t.push @@ Some Mirage_protocols_lwt.{address; network; gateway = None}
        | hd::_ -> t.push @@ Some Mirage_protocols_lwt.{address; network; gateway = (Some hd)}
      end
    end;
    t.dhcp_client <- status;
    Lwt.return_false

let rec listen t fn =  
  let listen_aux buffer = 
    match t.status with
    | Disconnected -> Lwt.return_unit
    | Look_for_IP  -> 
      begin
        dhcp_packet t buffer >>= function 
          | false -> Lwt.return_unit
          | true -> fn buffer
      end
    | IP_obtained  -> fn buffer
  in
  Netif.listen t.netif listen_aux

let rec write t = Netif.write t.netif
let writev t = Netif.writev t.netif
let mac t = Netif.mac t.netif
let get_stats_counters t = Netif.get_stats_counters t.netif
let reset_stats_counters t = Netif.reset_stats_counters t.netif


end