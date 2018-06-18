open Result
open Lwt.Infix
open Mirage_net

module Make(Netif_dhcp : Netif_dhcp.S) = struct

type +'a io = 'a Netif_dhcp.io

type t = Netif_dhcp.t

let connect (t, _) = Lwt.return t
let disconnect t = Netif_dhcp.disconnect t

type macaddr = Netif_dhcp.macaddr
type page_aligned_buffer = Netif_dhcp.page_aligned_buffer
type buffer = Netif_dhcp.buffer
type error = Netif_dhcp.error
let pp_error = Netif_dhcp.pp_error
let rec listen t fn = Netif_dhcp.listen t fn
let rec write t = Netif_dhcp.write t
let writev t = Netif_dhcp.writev t
let mac t = Netif_dhcp.mac t
let get_stats_counters t = Netif_dhcp.get_stats_counters t
let reset_stats_counters t = Netif_dhcp.reset_stats_counters t

end