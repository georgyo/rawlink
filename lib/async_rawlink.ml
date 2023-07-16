open! Core
open Async
open Deferred.Let_syntax
module Lowlevel = Rawlink_lowlevel

type t = {
  fd : Fd.t;
  reader : Reader.t;
  writer : Writer.t;
  packets : Cstruct.t list ref;
  buffer : Cstruct.t;
}

let dhcp_server_filter = Lowlevel.dhcp_server_filter
let dhcp_client_filter = Lowlevel.dhcp_client_filter

let open_link ?filter ?(promisc = false) ifname =
  let socket = Lowlevel.opensock ?filter ~promisc ifname in
  let () = Core_unix.set_nonblock socket in
  let fd =
    Fd.create (Fd.Kind.Socket `Active) socket
      (Info.of_string [%string "ifname-%{ifname}"])
  in
  {
    fd;
    reader = Reader.create fd;
    writer = Writer.create fd;
    packets = ref [];
    buffer = Cstruct.create 65536;
  }

let close_link t = Fd.close t.fd

let rec read_packet t =
  match !(t.packets) with
  | hd :: tl ->
      t.packets := tl;
      Deferred.return hd
  | [] ->
      let%bind read_result = Async_cstruct.read t.reader t.buffer in
      let (_ : unit Reader.Read_result.t) =
        Reader.Read_result.map read_result ~f:(fun n ->
            if n = 0 then failwith "Link socket closed";
            t.packets := Lowlevel.process_input t.buffer n)
      in
      read_packet t

let send_packet t (buf : Cstruct.t) =
  Async_cstruct.schedule_write t.writer buf;
  Writer.flushed t.writer
