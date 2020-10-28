// -----------------------------------------------------------------
// --------------------- INSTRUCTION TRACE -------------------------
// -----------------------------------------------------------------

// ------------------------ GLOBAL CONSTANTS -----------------------

uint64_t SYMBOLIC_POS_LEFT  = 0;
uint64_t SYMBOLIC_POS_RIGHT = 1;

uint64_t EXPRESSION_TYPE_POINT = 0;
uint64_t EXPRESSION_TYPE_RANGE = 1;

uint64_t* CONCRETE_T = (uint64_t*)0;

uint64_t MAX_SYMBOLIC_VARIABLES = 100;

// ------------------------ GLOBAL VARIABLES -----------------------

// TODO: This probably belongs into a context extension
uint64_t* trace = (uint64_t*) 0;

// TODO: This probably belongs into a context extension
uint64_t* beq_traces = (uint64_t*)0;

// TODO: This probably belongs into a context extension
uint64_t* reg_var_types = (uint64_t*)0;
uint64_t* mem_var_types = (uint64_t*)0;

uint64_t symbolic_id_seed = 1;

uint64_t allocate_symbolic_id() {
  uint64_t result;

  result = symbolic_id_seed;

  if (result >= MAX_SYMBOLIC_VARIABLES) {
    printf1("%s: reached symbolic variable limit \n", selfie_name);
    exit(EXITCODE_SYMBOLICEXECUTIONERROR);
  }

  symbolic_id_seed = symbolic_id_seed + 1;

  return result;
}

// instr_trace struct:
// +---+--------------------+
// | 0 | instruction        | The instruction ID that has been executed
// | 1 | prev_instruction   | The version of the symbolic operand
// | 2 | operand_snap       | Snapshot of the non-symbolic operand's value
// | 3 | symbolic_pos       | The position of the symbolic operand
// | 4 | next               | Pointer to the next entry of the linked list
// +---+--------------------+

uint64_t* allocate_instr_trace() {
  return smalloc(1 * SIZEOFUINT64STAR + 4 * SIZEOFUINT64);
}

uint64_t  get_instruction(uint64_t* trace)       { return             *trace; }
uint64_t* get_prev_instruction(uint64_t* trace) {
  return (uint64_t*)*(trace + 1);
}
uint64_t  get_operand_snapshot(uint64_t* trace)  { return             *(trace + 2); }
uint64_t  get_symbolic_position(uint64_t* trace) { return             *(trace + 3); }
uint64_t* get_next_instr_trace(uint64_t* trace)  { return (uint64_t*) *(trace + 4); }

void set_instruction(uint64_t* trace, uint64_t id)         { *trace        = id; }
void set_prev_instruction(uint64_t* trace, uint64_t* prev) {
  *(trace + 1) = (uint64_t)prev;
}
void set_operand_snapshot(uint64_t* trace, uint64_t value) { *(trace + 2)  = value; }
void set_symbolic_position(uint64_t* trace, uint64_t pos)  { *(trace + 3)  = pos; }
void set_next_instr_trace(uint64_t* trace, uint64_t* next) { *(trace + 4)  = (uint64_t) next; }

// symbolic type struct
// +---+---------+
// | 0 | type id | The instruction ID that has been executed
// | 1 | trace   | The instruction trace of the symbolic variable
// +---+---------+

uint64_t get_type_id(uint64_t* type) { return *type; }
uint64_t* get_trace(uint64_t* type) { return (uint64_t*)*(type + 1); }

void set_type_id(uint64_t* type, uint64_t id) { *type = id; }
void set_trace(uint64_t* type, uint64_t* trace) {
  *(type + 1) = (uint64_t)trace;
}

uint64_t* allocate_symbolic_type(uint64_t id) {
  uint64_t* type;

  type = smalloc(SIZEOFUINT64 + SIZEOFUINT64STAR);

  set_type_id(type, id);
  set_trace(type, (uint64_t*)0);

  return type;
}

void append_beq_trace(uint64_t* symbolic_type) {
  uint64_t* node;

  node = smalloc(2 * SIZEOFUINT64STAR);

  *node = (uint64_t)symbolic_type;
  *(node + 1) = (uint64_t)beq_traces;

  beq_traces = node;
}

uint64_t* append_instr_trace(uint64_t instruction, uint64_t* symbolic_type,
                             uint64_t operand_snapshot, uint64_t symbolic_pos) {
  uint64_t* current_trace;
  uint64_t* type;

  current_trace = allocate_instr_trace();

  type = allocate_symbolic_type(get_type_id(symbolic_type));
  set_trace(type, current_trace);

  set_instruction(current_trace, instruction);
  set_prev_instruction(current_trace, get_trace(symbolic_type));
  set_operand_snapshot(current_trace, operand_snapshot);
  set_symbolic_position(current_trace, symbolic_pos);
  set_next_instr_trace(current_trace, trace);

  *(trace + get_type_id(type)) = (uint64_t)current_trace;

  return type;
}

uint64_t get_trace_expression_type(uint64_t* trace) {
  while (trace != 0) {
    if (get_instruction(trace) == SLTU)
      return EXPRESSION_TYPE_RANGE;

    trace = get_prev_instruction(trace);
  }
  return EXPRESSION_TYPE_POINT;
}

uint64_t generate_test_case(uint64_t* trace, uint64_t positive_case) {
  uint64_t instruction;
  uint64_t negate;
  uint64_t sltu_pos;
  uint64_t sltu_value;

  // The first level must be a branch
  if (get_instruction(trace) != BEQ) {
    printf1("%s: instruction trace must begin with a branch instruction\n", selfie_name);
    exit(EXITCODE_BADARGUMENTS);
  }

  if (get_trace_expression_type(trace) == EXPRESSION_TYPE_RANGE) {
    // Look for the SLTU instruction
    // assert: all trace entries before SLTU are boolean operations (just values 0 and 1 are valid states)
    // TODO: Thoroughly handle boolean operations instead of just 1 - (...)
    negate = 0;
    while (get_instruction(trace) != SLTU) {
      // If we encounter a 1 - (...) trace entry before the SLTU instruction,
      // the range is negated.
      if (get_instruction(trace) == SUB) {
        if (get_operand_snapshot(trace) == 1) {
          if (get_symbolic_position(trace) == SYMBOLIC_POS_RIGHT)
            negate = 1;
          else
            negate = 0;
        } else
          negate = 0;
      } else
        negate = 0;

      trace = get_prev_instruction(trace);
    }

    sltu_value = get_operand_snapshot(trace);
    sltu_pos = get_symbolic_position(trace);

    // Perform inverse operation
    // TODO: Export to separate function
    trace = get_prev_instruction(trace);
    while (trace != 0) {
      instruction = get_instruction(trace);

      // TODO: MUL and DIVU need more handling
      if (instruction == ADD) {
        sltu_value = sltu_value - get_operand_snapshot(trace);
      } else if (instruction == ADDI) {
        sltu_value = sltu_value - get_operand_snapshot(trace);
      } else if (instruction == MUL) {
        sltu_value = sltu_value / get_operand_snapshot(trace);
      } else if (instruction == SUB) {
        if (get_symbolic_position(trace) == SYMBOLIC_POS_RIGHT) {
          // operand - x < sltu_value <-*(-1)->  x - operand > -sltu_value <-> x > operand - sltu_value
          if (sltu_pos == SYMBOLIC_POS_RIGHT)
            sltu_pos = SYMBOLIC_POS_LEFT;
          else
            sltu_pos = SYMBOLIC_POS_RIGHT;

          sltu_value = get_operand_snapshot(trace) - sltu_value;
        } else {
          // x - operand
          sltu_value = sltu_value + get_operand_snapshot(trace);
        }
      } else if (instruction == DIVU) {
        if (get_symbolic_position(trace) == SYMBOLIC_POS_RIGHT) {
          // operand / x < sltu_value <-> x / operand > 1 / sltu_value <-> x > operand / sltu_value
          if (sltu_pos == SYMBOLIC_POS_RIGHT)
            sltu_pos = SYMBOLIC_POS_LEFT;
          else
            sltu_pos = SYMBOLIC_POS_RIGHT;

          sltu_value = get_operand_snapshot(trace) / sltu_value;
        } else {
          // x / operand
          sltu_value = sltu_value * get_operand_snapshot(trace);
        }
      }

      trace = get_prev_instruction(trace);
    }

    if (positive_case) {
      if (negate) {
        if (sltu_pos == SYMBOLIC_POS_LEFT) {
          // x >= sltu_value
          return sltu_value;
        } else {
          // x <= sltu_value
          return sltu_value;
        }
      } else {
        if (sltu_pos == SYMBOLIC_POS_LEFT) {
          // x < sltu_value
          return sltu_value - 1;
        } else {
          // x > sltu_value
          return sltu_value + 1;
        }
      }
    } else {
      if (negate) {
        if (sltu_pos == SYMBOLIC_POS_LEFT) {
          // x >= sltu_value
          return sltu_value - 1;
        } else {
          // x <= sltu_value
          return sltu_value + 1;
        }
      } else {
        if (sltu_pos == SYMBOLIC_POS_LEFT) {
          // x < sltu_value
          return sltu_value;
        } else {
          // x > sltu_value
          return sltu_value;
        }
      }
    }
  } else {
    // TODO: Seldom - requires if() without comparison expression
    printf1("%s: warning: point-type expression not implemented yet", selfie_name);
    return 0;
  }
}

// -----------------------------------------------------------------
// ---------------------- CONCOLIC ENGINE --------------------------
// -----------------------------------------------------------------

void reset_concolic_engine();
void run_until_exception_concolic();
void execute_concolic();
uint64_t *concolic_switch(uint64_t *to_context, uint64_t timeout);

void reset_concolic_engine() {
  uint64_t i;
  uint64_t amount_of_words;

  reg_var_types = smalloc(32 * SIZEOFUINT64);

  i = 0;

  while (i < 32) {
    *(reg_var_types + i) = (uint64_t)CONCRETE_T;

    i = i + 1;
  }

  mem_var_types = smalloc(4096 * MEGABYTE);

  i = 0;

  amount_of_words = 4096 * MEGABYTE / SIZEOFUINT64;

  while (i < amount_of_words) {
    *(mem_var_types + i) = (uint64_t)CONCRETE_T;

    i = i + 1;
  }

  trace = smalloc(MAX_SYMBOLIC_VARIABLES * SIZEOFUINT64STAR);
}

void run_until_exception_concolic() {
  trap = 0;

  while (trap == 0) {
    fetch();
    decode();
    execute_concolic();

    interrupt();
  }

  trap = 0;
}

uint64_t *concolic_switch(uint64_t *to_context, uint64_t timeout) {
  current_context = do_switch(current_context, to_context, timeout);

  run_until_exception_concolic();

  save_context(current_context);

  return current_context;
}

uint64_t* get_reg_var_type(uint64_t reg) {
  return (uint64_t*)*(reg_var_types + reg);
}

void set_reg_var_type(uint64_t reg, uint64_t* type) {
  if (reg != REG_ZR)
    *(reg_var_types + reg) = (uint64_t)type;
}

void set_mem_var_type(uint64_t vaddr, uint64_t* type) {
  uint64_t idx;

  idx = vaddr / SIZEOFUINT64;

  *(mem_var_types + idx) = (uint64_t)type;
}

uint64_t* get_mem_var_type(uint64_t addr) {
  uint64_t idx;

  idx = addr / SIZEOFUINT64;

  return (uint64_t*)*(mem_var_types + idx);
}

void track_rtype_instruction() {
  uint64_t* rs1_type;
  uint64_t* rs2_type;
  uint64_t rs1_value;
  uint64_t rs2_value;
  uint64_t rd_type;

  rs1_type = get_reg_var_type(rs1);
  rs2_type = get_reg_var_type(rs2);

  rs1_value = *(registers + rs1);
  rs2_value = *(registers + rs2);

  rd_type = 0;

  if (rs1_type == CONCRETE_T) {
    if (rs2_type == CONCRETE_T)
      set_reg_var_type(rd, CONCRETE_T);
    else {
      set_reg_var_type(
          rd, append_instr_trace(is, rs2_type, rs1_value, SYMBOLIC_POS_RIGHT));
    }
  } else {
    if (rs2_type == CONCRETE_T) {
      set_reg_var_type(
          rd, append_instr_trace(is, rs1_type, rs2_value, SYMBOLIC_POS_LEFT));
    } else {
      printf1("%s: handle two symbolic types in rtype instruction\n",
              selfie_name);
      exit(EXITCODE_SYMBOLICEXECUTIONERROR);
    }
  }
}

void track_concrete_rd() { set_reg_var_type(rd, CONCRETE_T); }

void track_itype_instruction() {
  uint64_t* rs1_type;

  rs1_type = get_reg_var_type(rs1);

  if (rs1_type == CONCRETE_T)
    set_reg_var_type(rd, CONCRETE_T);
  else
    set_reg_var_type(rd,
                     append_instr_trace(is, rs1_type, imm, SYMBOLIC_POS_LEFT));
}

void track_ld() {
  uint64_t vaddr;

  vaddr = *(registers + rs1) + imm;

  set_reg_var_type(rd, get_mem_var_type(vaddr));
}

void track_sd() {
  uint64_t vaddr;

  vaddr = *(registers + rs1) + imm;

  set_mem_var_type(vaddr, get_reg_var_type(rs2));
}

void track_beq() {
  uint64_t* rs1_type;
  uint64_t* rs2_type;
  uint64_t rs1_value;
  uint64_t rs2_value;

  rs1_type = get_reg_var_type(rs1);
  rs2_type = get_reg_var_type(rs2);

  rs1_value = *(registers + rs1);
  rs2_value = *(registers + rs2);

  // We don't need to backtrace concrete-concrete branches
  if (rs1_type == CONCRETE_T) {
    if (rs2_type != CONCRETE_T)
      append_beq_trace(
          append_instr_trace(is, rs2_type, rs1_value, SYMBOLIC_POS_RIGHT));
  } else {
    if (rs2_type == CONCRETE_T)
      append_beq_trace(
          append_instr_trace(is, rs1_type, rs2_value, SYMBOLIC_POS_LEFT));
    else {
      printf1("%s: handle two symbolic types in rtype instruction\n",
              selfie_name);
      exit(EXITCODE_SYMBOLICEXECUTIONERROR);
    }
  }
}

void execute_concolic() {
  if (is == ADDI) {
    track_itype_instruction();
    do_addi();
  } else if (is == LD) {
    track_ld();
    do_ld();
  } else if (is == SD) {
    track_sd();
    do_sd();
  } else if (is == ADD) {
    track_rtype_instruction();
    do_add();
  } else if (is == SUB) {
    track_rtype_instruction();
    do_sub();
  } else if (is == MUL) {
    track_rtype_instruction();
    do_mul();
  } else if (is == DIVU) {
    track_rtype_instruction();
    do_divu();
  } else if (is == REMU) {
    track_rtype_instruction();
    do_remu();
  } else if (is == SLTU) {
    track_rtype_instruction();
    do_sltu();
  } else if (is == BEQ) {
    // does not change types in memory
    // but must be recorded for backtracing
    track_beq();
    do_beq();
  } else if (is == JAL) {
    track_concrete_rd();
    do_jal();
  } else if (is == JALR) {
    track_concrete_rd();
    do_jalr();
  } else if (is == LUI) {
    track_concrete_rd();
    do_lui();
  } else if (is == ECALL)
    do_ecall();
}


void implement_concolic_read(uint64_t* context) {
  // parameters
  uint64_t fd;
  uint64_t vbuffer;
  uint64_t size;

  // local variables
  uint64_t failed;
  uint64_t actually_read;
  uint64_t* type;

  if (debug_syscalls) {
    print("(concolic read): ");
    print_register_value(REG_A0);
    print(",");
    print_register_hexadecimal(REG_A1);
    print(",");
    print_register_value(REG_A2);
    print(" |- ");
    print_register_value(REG_A0);
  }

  fd = *(get_regs(context) + REG_A0);
  vbuffer = *(get_regs(context) + REG_A1);
  size = *(get_regs(context) + REG_A2);

  if (fd != 0) {
    printf1("%s: error in read(): can only read from stdin\n", selfie_name);
    exit(EXITCODE_SYMBOLICEXECUTIONERROR);
  }

  if (size % SIZEOFUINT64 != 0) {
    printf1("%s: error in read(): can only read bytes streams with len "
            "% 8 == 0",
            selfie_name);
    exit(EXITCODE_SYMBOLICEXECUTIONERROR);
  }

  failed = 0;

  actually_read = 0;

  while (actually_read < size) {
    if (is_valid_virtual_address(vbuffer))
      if (is_valid_data_stack_heap_address(context, vbuffer))
        if (is_virtual_address_mapped(get_pt(context), vbuffer)) {
          type = allocate_symbolic_type(allocate_symbolic_id());

          set_mem_var_type(vbuffer, type);

          // TODO: Initialize this with random values
          store_virtual_memory(get_pt(context), vbuffer, 10);
        } else {
          failed = 1;

          size = 0;

          printf2("%s: reading into virtual address %p failed because the "
                  "address is unmapped\n",
                  selfie_name, (char*)vbuffer);
        }
      else {
        failed = 1;

        size = 0;

        printf2("%s: reading into virtual address %p failed because the "
                "address is in an invalid segment\n",
                selfie_name, (char*)vbuffer);
      }
    else {
      failed = 1;

      size = 0;

      printf2("%s: reading into virtual address %p failed because the address "
              "is invalid\n",
              selfie_name, (char*)vbuffer);
    }

    vbuffer = vbuffer + SIZEOFUINT64;

    actually_read = actually_read + SIZEOFUINT64;
  }

  if (failed)
    *(get_regs(context) + REG_A0) = sign_shrink(-1, SYSCALL_BITWIDTH);
  else
    *(get_regs(context) + REG_A0) = size;

  set_pc(context, get_pc(context) + INSTRUCTIONSIZE);

  if (debug_read)
    printf3("%s: actually read %u bytes from file with descriptor %u\n",
            selfie_name, (char*)size, (char*)fd);

  if (debug_syscalls) {
    print(" -> ");
    print_register_value(REG_A0);
    println();
  }
}

uint64_t handle_concolic_system_call(uint64_t* context) {
  uint64_t a7;

  set_exception(context, EXCEPTION_NOEXCEPTION);

  a7 = *(get_regs(context) + REG_A7);

  if (a7 == SYSCALL_BRK) {
    if (get_gc_enabled_gc(context))
      implement_gc_brk(context);
    else
      implement_brk(context);
  } else if (a7 == SYSCALL_READ)
    implement_concolic_read(context);
  else if (a7 == SYSCALL_WRITE)
    implement_write(context);
  else if (a7 == SYSCALL_OPENAT)
    implement_openat(context);
  else if (a7 == SYSCALL_EXIT) {
    implement_exit(context);

    // TODO: exit only if all contexts have exited
    return EXIT;
  } else {
    printf2("%s: unknown system call %u\n", selfie_name, (char*)a7);

    set_exit_code(context, EXITCODE_UNKNOWNSYSCALL);

    return EXIT;
  }

  return DONOTEXIT;
}

uint64_t handle_concolic_exception(uint64_t* context) {
  uint64_t exception;

  exception = get_exception(context);

  if (exception == EXCEPTION_SYSCALL)
    return handle_concolic_system_call(context);
  else if (exception == EXCEPTION_PAGEFAULT)
    return handle_page_fault(context);
  else if (exception == EXCEPTION_DIVISIONBYZERO)
    return handle_division_by_zero(context);
  else if (exception == EXCEPTION_TIMER)
    return handle_timer(context);
  else {
    printf2("%s: context %s threw uncaught exception: ", selfie_name,
            get_name(context));
    print_exception(exception, get_fault(context));
    println();

    set_exit_code(context, EXITCODE_UNCAUGHTEXCEPTION);

    return EXIT;
  }
}

void print_operator(uint64_t is) {
  // assert: 1 <= is <= number of RISC-U instructions
  if (is == ADDI)
    print("+");
  else if (is == ADD)
    print("+");
  else if (is == SUB)
    print("-");
  else if (is == MUL)
    print("*");
  else if (is == DIVU)
    print("/");
  else if (is == REMU)
    print("%");
  else if (is == SLTU)
    print("<");
  else if (is == BEQ)
    print("==");
}

void print_trace(uint64_t* trace) {
  if (trace == (uint64_t*)0) {
    print("x");

    return;
  }

  print("(");

  if (get_symbolic_position(trace) == SYMBOLIC_POS_LEFT)
    print_trace(get_prev_instruction(trace));
  else
    print_unsigned_integer(get_operand_snapshot(trace));

  print(" ");
  print_operator(get_instruction(trace));
  print(" ");

  if (get_symbolic_position(trace) == SYMBOLIC_POS_LEFT)
    print_unsigned_integer(get_operand_snapshot(trace));
  else
    print_trace(get_prev_instruction(trace));

  print(")");
}

void print_beq_traces(uint64_t* traces) {
  uint64_t symbolic_id;
  uint64_t* type;
  uint64_t* trace;

  if (traces == (uint64_t*)0)
    return;

  type = (uint64_t*)*traces;
  symbolic_id = get_type_id(type);
  trace = get_trace(type);

  print_beq_traces((uint64_t*)*(traces + 1));

  print("beq trace ");
  print_unsigned_integer(symbolic_id);
  print(": ");
  print_trace(trace);

  print(" (test case ");
  print_unsigned_integer(generate_test_case(trace, 1));
  print(", negative ");
  print_unsigned_integer(generate_test_case(trace, 0));
  print(")");

  println();
}

uint64_t concolic(uint64_t *to_context) {
  uint64_t timeout;
  uint64_t *from_context;

  print("concolic\n");
  printf1("%s: "
          ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
          ">>>>>>>>>>>>\n",
          selfie_name);

  timeout = TIMESLICE;

  while (1) {
    from_context = concolic_switch(to_context, timeout);

    if (get_parent(from_context) != MY_CONTEXT) {
      // switch to parent which is in charge of handling exceptions
      to_context = get_parent(from_context);

      timeout = TIMEROFF;
    } else if (handle_concolic_exception(from_context) == EXIT)
      return get_exit_code(from_context);
    else {
      // TODO: scheduler should go here
      to_context = from_context;

      timeout = TIMESLICE;
    }
  }
}

uint64_t selfie_run_concolically() {
  uint64_t memory_size;

  if (string_compare(argument, "-")) {
    if (number_of_remaining_arguments() > 0) {
      memory_size = atoi(peek_argument(0));

      if (binary_length == 0) {
        printf1("%s: nothing to run symbolically\n", selfie_name);

        return EXITCODE_BADARGUMENTS;
      }

      reset_interpreter();
      reset_profiler();
      reset_microkernel();
      reset_concolic_engine();

      init_memory(memory_size);

      current_context = create_context(MY_CONTEXT, 0);

      // assert: number_of_remaining_arguments() > 0

      boot_loader(current_context);

      printf3("%s: concolically executing %s with %uMB physical memory\n",
              selfie_name, binary_name,
              (char *)(total_page_frame_memory / MEGABYTE));

      run = 1;

      concolic(current_context);

      run = 0;

      printf2("%s: terminating %s\n", selfie_name, get_name(current_context));

      print_profile(current_context);

      print_beq_traces(beq_traces);

      return EXITCODE_NOERROR;
    } else
      return EXITCODE_BADARGUMENTS;
  } else
    return EXITCODE_BADARGUMENTS;
}
// -----------------------------------------------------------------
// ----------------------------- MAIN ------------------------------
// -----------------------------------------------------------------

int main(int argc, char **argv) {
  uint64_t exit_code;

  init_selfie((uint64_t)argc, (uint64_t *)argv);

  init_library();

  init_system();

  exit_code = selfie(1);

  if (exit_code == EXITCODE_MOREARGUMENTS)
    exit_code = selfie_run_concolically();

  return exit_selfie(exit_code, " - ");
}
