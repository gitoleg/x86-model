open Core_kernel.Std
open Bap.Std
open Bap_rtl.Std
open X86_env

type cpu = {
  reg         : (op -> exp) ec;
  load        : exp -> bitwidth -> exp;
  store       : exp -> exp -> bitwidth -> rtl;
  word_width  : bitwidth;
  word_width' : exp;
  rsp         : exp;
}

include Model.Lifter(struct
    type t = cpu
  end)

module R32 = struct
  open Model
  open R32

  let model = Reg.create ()

  let () =
    let seg = Cls.of_string "seg" in
    let add = Reg.add_reg model in
    List.iter ~f:(add Cls.gpr) [rbp;rsp;rsi;rdi;rip;rax;rbx;rcx;rdx;];
    List.iter ~f:(add seg) [fs_base; gs_base];
    Array.iter ~f:(add Cls.vector) ymms;

  module M = Mem.Make(struct
      let mem = R32.mem
      let endian = LittleEndian
    end)

  let cpu = {
    reg = Reg.reg_ec model;
    load = M.load;
    store = M.store;
    word_width  = word;
    word_width' = Exp.of_word (Word.of_int ~width:32 32);
    rsp = Exp.of_var rsp;
  }

  let () = init cpu

end

let push_r cpu ops =
  let src = unsigned cpu.reg ops.(0) in
  let tmp = unsigned var cpu.word_width in
  let bytes = unsigned const word 8 in
  RTL.[
    tmp := src;
    src := cpu.rsp - cpu.word_width' / bytes;
    cpu.store cpu.rsp tmp word;
  ]

let push_i cpu ops =
  let src = unsigned imm ops.(0) in
  let tmp = unsigned var cpu.word_width in
  let bytes = unsigned const word 8 in
  RTL.[
    tmp := src;
    cpu.rsp := cpu.rsp - cpu.word_width' / bytes;
    cpu.store cpu.rsp tmp word;
  ]

let push_rmm cpu ops =
  let src = unsigned imm ops.(3) in
  let tmp = unsigned var cpu.word_width in
  let bytes = unsigned const word 8 in
  RTL.[
    tmp := cpu.load src word;
    cpu.rsp := cpu.rsp - cpu.word_width' / bytes;
    cpu.store cpu.rsp tmp word;
  ]

let () = register "PUSH32r" push_r
let () = register "PUSHi32" push_i
let () = register "PUSH32rmm" push_rmm

(** push
    mov
    test
    je
    call
    add
    pop
    lea
    ret
    sub
    xor
    and
    hlt
    xchg
    movl
    leave
    sar
    shl
    shr
    cmpb
    movb
    repz
    jmp
    nop
    movzbl
    movsbl
    imul
  *)
