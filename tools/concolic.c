/*#include "../selfie.h"*/

// -----------------------------------------------------------------
// --------------------- INSTRUCTION TRACE -------------------------
// -----------------------------------------------------------------

// ------------------------ GLOBAL CONSTANTS -----------------------

uint64_t SYMBOLIC_POS_LEFT  = 0;
uint64_t SYMBOLIC_POS_RIGHT = 1;

// ------------------------ GLOBAL VARIABLES -----------------------

// TODO: This probably belongs into a context extension
uint64_t* trace = (uint64_t*) 0;

// instr_trace struct:
// +---+---------------+
// | 0 | instruction   | The instruction ID that has been executed
// | 1 | symbolic_id   | The ID of the symbolic operand
// | 2 | operand_snap  | Snapshot of the non-symbolic operand's value
// | 3 | symbolic_pos  | The position of the symbolic operand
// | 4 | next          | Pointer to the next entry of the linked list
// +---+---------------+

uint64_t* allocate_instr_trace() {
  return smalloc(1 * SIZEOFUINT64STAR + 4 * SIZEOFUINT64);
}

uint64_t  get_instruction(uint64_t* trace)       { return             *trace; }
uint64_t  get_symbolic_id(uint64_t* trace)       { return             *(trace + 1); }
uint64_t  get_operand_snapshot(uint64_t* trace)  { return             *(trace + 2); }
uint64_t  get_symbolic_position(uint64_t* trace) { return             *(trace + 3); }
uint64_t* get_next_instr_trace(uint64_t* trace)  { return (uint64_t*) *(trace + 4); }

void set_instruction(uint64_t* trace, uint64_t id)         { *trace        = id; }
void set_symbolic_id(uint64_t* trace, uint64_t id)         { *(trace + 1)  = id; }
void set_operand_snapshot(uint64_t* trace, uint64_t value) { *(trace + 2)  = value; }
void set_symbolic_position(uint64_t* trace, uint64_t pos)  { *(trace + 3)  = pos; }
void set_next_instr_trace(uint64_t* trace, uint64_t* next) { *(trace + 4)  = (uint64_t) next; }

void append_instr_trace(uint64_t instruction, uint64_t symbolic_id, uint64_t operand_snapshot, uint64_t symbolic_pos) {
  uint64_t* current_trace;
  current_trace = allocate_instr_trace();

  set_instruction(current_trace, instruction);
  set_symbolic_id(current_trace, symbolic_id);
  set_operand_snapshot(current_trace, operand_snapshot);
  set_symbolic_position(current_trace, symbolic_pos);
  set_next_instr_trace(current_trace, trace);

  trace = current_trace;
}

// -----------------------------------------------------------------
// ---------------------- CONCOLIC ENGINE --------------------------
// -----------------------------------------------------------------

void run_until_exception_concolic();
void execute_concolic();
uint64_t *concolic_switch(uint64_t *to_context, uint64_t timeout);

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

uint64_t *reg_var_types = (uint64_t *)0;
uint64_t *mem_var_types = (uint64_t *)0;

uint64_t get_reg_var_type(uint64_t reg) { return *(reg_var_types + reg); }

void set_reg_var_type(uint64_t reg, uint64_t type) {
  if (reg != REG_ZR)
    *(reg_var_types + reg) = type;
}

void set_mem_var_type(uint64_t vaddr, uint64_t type) {
  uint64_t idx;

  idx = vaddr / SIZEOFUINT64;

  *(mem_var_types + idx) = type;
}

uint64_t CONCRETE_T = 0;

uint64_t get_mem_var_type(uint64_t addr) {
  uint64_t idx;

  idx = addr / SIZEOFUINT64;

  return *(mem_var_types + idx);
}

void track_rtype_instruction() {
  uint64_t rs1_type;
  uint64_t rs2_type;
  uint64_t rs1_value;
  uint64_t rs2_value;

  rs1_type = get_reg_var_type(rs1);
  rs2_type = get_reg_var_type(rs2);

  rs1_value = *(registers + rs1);
  rs2_value = *(registers + rs2);

  if (rs1_type == CONCRETE_T) {
    if (rs2_type == CONCRETE_T)
      set_reg_var_type(rd, CONCRETE_T);
    else {
      set_reg_var_type(rd, rs2_type);
      append_instr_trace(is, rs2_type, rs1_value, SYMBOLIC_POS_RIGHT);
    }
  } else {
    if (rs2_type == CONCRETE_T) {
      set_reg_var_type(rd, rs1_type);
      append_instr_trace(is, rs1_type, rs2_value, SYMBOLIC_POS_LEFT);
    } else {
      printf1("%s: handle two symbolic types in rtype instruction\n",
              selfie_name);
      exit(1);
    }
  }
}

void track_concrete_rd() { set_reg_var_type(rd, CONCRETE_T); }

void track_itype_instruction() {
  uint64_t rs1_type;

  rs1_type = get_reg_var_type(rs1);

  if (rs1_type == CONCRETE_T)
    set_reg_var_type(rd, CONCRETE_T);
  else {
    set_reg_var_type(rd, rs1_type);
    append_instr_trace(is, rs1_type, imm, SYMBOLIC_POS_LEFT);
  }
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
  uint64_t rs1_type;
  uint64_t rs2_type;
  uint64_t rs1_value;
  uint64_t rs2_value;

  rs1_type = get_reg_var_type(rs1);
  rs2_type = get_reg_var_type(rs2);

  rs1_value = *(registers + rs1);
  rs2_value = *(registers + rs2);

  // We don't need to backtrace concrete-concrete branches
  if (rs1_type == CONCRETE_T) {
    if (rs2_type != CONCRETE_T)
      append_instr_trace(is, rs2_type, rs1_value, SYMBOLIC_POS_RIGHT);
  } else {
    if (rs2_type == CONCRETE_T)
      append_instr_trace(is, rs1_type, rs2_value, SYMBOLIC_POS_LEFT);
    else {
      printf1("%s: handle two symbolic types in rtype instruction\n",
              selfie_name);
      exit(1);
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
    } else if (handle_exception(from_context) == EXIT)
      return get_exit_code(from_context);
    else {
      // TODO: scheduler should go here
      to_context = from_context;

      timeout = TIMESLICE;
    }
  }
}

uint64_t* create_concolic_context(uint64_t* parent, uint64_t* vctxt) {
  uint64_t* context;
  uint64_t i;
  uint64_t amount_of_words;

  context = new_context();

  init_context(context, parent, vctxt);

  set_next_context(context, (uint64_t *)0);
  /*set_execution_depth(context, 0);*/
  /*set_path_condition(context, "true");*/
  /*set_symbolic_memory(context, (uint64_t *)0);*/
  /*set_symbolic_regs(context, zmalloc(NUMBEROFREGISTERS * REGISTERSIZE));*/
  /*set_beq_counter(context, 0);*/
  /*set_merge_partner(context, (uint64_t *)0);*/
  /*set_call_stack(context, call_stack_tree);*/

  reg_var_types = smalloc(32 * SIZEOFUINT64);

  i = 0;

  while (i < 32) {
    *(reg_var_types + i) = CONCRETE_T;

    i = i + 1;
  }

  mem_var_types = smalloc(4096 * MEGABYTE);

  i = 0;

  amount_of_words = 4096 * MEGABYTE / SIZEOFUINT64;

  while (i < amount_of_words) {
    *(mem_var_types + i) = CONCRETE_T;

    i = i + 1;
  }

  if (debug_create)
    printf3("%s: parent context %p created child context %p\n", selfie_name,
            (char *)parent, (char *)used_contexts);

  return context;
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

      init_memory(memory_size);

      current_context = create_concolic_context(MY_CONTEXT, 0);

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
