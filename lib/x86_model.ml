open Core_kernel.Std
open Bap.Std
open Bap_rtl.Std
open X86_env

module Reg_op = Reg

type cpu = {
  reg         : (op -> exp) ec;
  reg_or_nil  : (op -> exp) ec;
  load        : exp -> bitwidth -> exp;
  store       : exp -> exp -> bitwidth -> rtl;
  word_width  : bitwidth;
  word_width' : exp;
  sp          : exp;
  rax         : exp;
}

include Model.Lifter(struct
    type t = cpu
  end)

module R32 = struct
  open Model
  open R32

  let model = Reg.create ()

  let al = low byte (Exp.of_var rax)

  let () =
    let seg = Cls.of_string "seg" in
    let add = Reg.add_reg model in
    List.iter ~f:(add Cls.gpr) [rbp;rsp;rsi;rdi;rip;rax;rbx;rcx;rdx];
    Reg.add_exp model Cls.gpr (`Name "AL") al;
    List.iter ~f:(add seg) [fs_base; gs_base];
    Array.iter ~f:(add Cls.vector) ymms;

  module M = Mem.Make(struct
      let mem = R32.mem
      let endian = LittleEndian
    end)

  let reg_or_nil =
    let find reg =
      let name = `Name (Reg_op.name reg) in
      match Reg.exp model name with
      | Some x -> x
      | None -> Exp.of_word (Word.zero 32) in
    reg find


  let cpu = {
    reg = Reg.reg_ec model;
    reg_or_nil;
    load = M.load;
    store = M.store;
    word_width  = word;
    word_width' = Exp.of_word (Word.of_int ~width:32 32);
    sp = Exp.of_var rsp;
    rax = Exp.of_var rax;
  }

  let () = init cpu

end

let push32_r cpu ops =
  let src = unsigned cpu.reg ops.(0) in
  let tmp = unsigned var cpu.word_width in
  let bytes = unsigned const word 8 in
  RTL.[
    tmp := src;
    cpu.sp := cpu.sp - cpu.word_width' / bytes;
    cpu.store cpu.sp tmp word;
  ]

let push32_i cpu ops =
  let src = unsigned imm ops.(0) in
  let tmp = unsigned var cpu.word_width in
  let bytes = unsigned const word 8 in
  RTL.[
    tmp := src;
    cpu.sp := cpu.sp - cpu.word_width' / bytes;
    cpu.store cpu.sp tmp word;
  ]

let push32_rmm cpu ops =
  let base  = unsigned R32.reg_or_nil ops.(0) in
  let index = unsigned imm ops.(1) in
  let scale = unsigned cpu.reg ops.(2) in
  let disp  = unsigned imm ops.(3) in
  let tmp = unsigned var cpu.word_width in
  let bytes = unsigned const word 8 in
  RTL.[
    tmp := cpu.load (base + scale * index + disp) word;
    cpu.sp := cpu.sp - cpu.word_width' / bytes;
    cpu.store cpu.sp tmp word;
  ]

let () = register "PUSH32r" push32_r
let () = register "PUSHi32" push32_i
let () = register "PUSH32rmm" push32_rmm


let mov32_rr cpu ops =
  let dst = unsigned cpu.reg ops.(0) in
  let src = unsigned cpu.reg ops.(1) in
  RTL.[ dst := src ]

let mov32_rm cpu ops =
  let dst = unsigned cpu.reg ops.(0) in
  let base = unsigned cpu.reg_or_nil ops.(1) in
  let index = unsigned imm ops.(2) in
  let scale = unsigned cpu.reg_or_nil ops.(3) in
  let disp  = unsigned imm ops.(4) in
  RTL.[
    dst := cpu.load (base + scale * index + disp) word;
  ]

let mov32_ri cpu ops =
  let dst = unsigned cpu.reg ops.(0) in
  let src = unsigned imm ops.(1) in
  RTL.[ dst := src ]

let mov32_mi cpu ops =
  let base = unsigned cpu.reg_or_nil ops.(0) in
  let index = unsigned imm ops.(1) in
  let scale = unsigned cpu.reg_or_nil ops.(2) in
  let disp  = unsigned imm ops.(3) in
  let x = unsigned imm ops.(5) in
  RTL.[
    cpu.store (base + scale * index + disp) x word ;
  ]

let mov32_mr cpu ops =
  let base = unsigned cpu.reg_or_nil ops.(0) in
  let index = unsigned imm ops.(1) in
  let scale = unsigned cpu.reg_or_nil ops.(2) in
  let disp  = unsigned imm ops.(3) in
  let x = unsigned cpu.reg ops.(5) in
  RTL.[
    cpu.store (base + scale * index + disp) x word ;
  ]

let mov8_mi cpu ops =
  let base = unsigned cpu.reg_or_nil ops.(0) in
  let index = unsigned imm ops.(1) in
  let scale = unsigned cpu.reg_or_nil ops.(2) in
  let disp  = unsigned imm ops.(3) in
  let x = unsigned imm ops.(5) in
  RTL.[
    cpu.store (base + scale * index + disp) x byte;
  ]

let mov32_ao_32 cpu ops =
  let src = unsigned imm ops.(0) in
  RTL.[
    cpu.rax := cpu.load src word;
  ]

let movzx32rm8 cpu ops =
  let dst = unsigned cpu.reg ops.(0) in
  let base = unsigned cpu.reg_or_nil ops.(1) in
  let index = unsigned imm ops.(2) in
  let scale = unsigned cpu.reg_or_nil ops.(3) in
  let disp  = unsigned imm ops.(4) in
  RTL.[
    dst := cpu.load (base + scale * index + disp) byte;
  ]

let movsx32rr8 cpu ops =
  let dst = signed cpu.reg ops.(0) in
  let src = signed cpu.reg ops.(1) in
  RTL.[
    dst := src;
  ]

let () = register "MOV32rr" mov32_rr
let () = register "MOV32rm" mov32_rm
let () = register "MOV32ri" mov32_ri
let () = register "MOV32mi" mov32_mi
let () = register "MOV32mr" mov32_mr
let () = register "MOV8mi" mov8_mi
let () = register "MOV32ao32" mov32_ao_32
let () = register "MOVZX32rm8" movzx32rm8
let () = register "MOVSX32rr8" movsx32rr8

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
