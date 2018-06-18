open Result
open Lwt.Infix
open Mirage_net

module Make(Netif_dhcp : Netif_dhcp.S) = struct

type t = Mirage_protocols_lwt.ipv4_config Lwt_stream.t
let connect (_, t) = Lwt.return t


end