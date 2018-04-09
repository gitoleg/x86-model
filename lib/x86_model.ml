open Core_kernel.Std
open Bap.Std
open Bap_rtl.Std
open X86_env

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

  type cpu = {
    reg     : (op -> exp) ec;
    load    : exp -> bitwidth -> exp;
    store   : exp -> exp -> bitwidth -> rtl;
  }

  let cpu = {
    reg = Reg.reg_ec model;
    load = M.load;
    store = M.store;
  }

end





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
