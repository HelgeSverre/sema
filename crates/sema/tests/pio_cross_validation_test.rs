//! Cross-validation of Sema's PIO assembler against the `pio` Rust crate.
//!
//! The `pio` crate (v0.3) is the reference PIO assembler used by rp2040-hal.
//! For each test, we:
//!   1. Build a PIO instruction using the `pio` crate's Assembler
//!   2. Build the same instruction using Sema's `pio/*` functions via eval
//!   3. Assert byte-for-byte equality
//!
//! This validates encoding correctness against a known-good implementation.

use pio::{
    Assembler, InSource, InstructionOperands, IrqIndexMode, JmpCondition, MovDestination,
    MovOperation, MovSource, OutDestination, SetDestination, SideSet, WaitSource,
};
use sema_core::Value;
use sema_eval::Interpreter;

/// Helper: encode a single PIO instruction via the pio crate and return LE bytes.
fn pio_reference(operands: InstructionOperands, delay: u8) -> Vec<u8> {
    let ss = SideSet::new(false, 0, false);
    let instr = pio::Instruction {
        operands,
        delay,
        side_set: None,
    };
    let word = instr.encode(ss);
    vec![(word & 0xFF) as u8, (word >> 8) as u8]
}

/// Helper: encode via pio crate's Assembler (handles labels, wrap, etc.)
fn pio_assemble_reference(build: impl FnOnce(&mut Assembler<32>)) -> Vec<u8> {
    let mut a = Assembler::<32>::new();
    build(&mut a);
    let program = a.assemble_program();
    let mut bytes = Vec::new();
    for &word in &program.code {
        bytes.push((word & 0xFF) as u8);
        bytes.push((word >> 8) as u8);
    }
    bytes
}

/// Helper: evaluate a Sema expression and extract the :instructions bytevector.
fn sema_assemble(expr: &str) -> Vec<u8> {
    let interp = Interpreter::new();
    let full = format!("(get (pio/assemble {expr}) :instructions)");
    let result = interp
        .eval_str(&full)
        .unwrap_or_else(|e| panic!("Sema eval failed for: {full}\nError: {e}"));
    result
        .as_bytevector()
        .unwrap_or_else(|| panic!("Expected bytevector from: {full}\nGot: {result}"))
        .to_vec()
}

/// Helper: evaluate a single Sema PIO instruction (no labels needed).
fn sema_single(instr: &str) -> Vec<u8> {
    sema_assemble(&format!("(list {instr})"))
}

// ═══════════════════════════════════════════════════════════════════
// SET instructions (opcode 111)
// ═══════════════════════════════════════════════════════════════════

#[test]
fn xval_set_pins_0() {
    let reference = pio_reference(
        InstructionOperands::SET {
            destination: SetDestination::PINS,
            data: 0,
        },
        0,
    );
    assert_eq!(sema_single("(pio/set :pins 0)"), reference);
}

#[test]
fn xval_set_pins_1() {
    let reference = pio_reference(
        InstructionOperands::SET {
            destination: SetDestination::PINS,
            data: 1,
        },
        0,
    );
    assert_eq!(sema_single("(pio/set :pins 1)"), reference);
}

#[test]
fn xval_set_pins_31() {
    let reference = pio_reference(
        InstructionOperands::SET {
            destination: SetDestination::PINS,
            data: 31,
        },
        0,
    );
    assert_eq!(sema_single("(pio/set :pins 31)"), reference);
}

#[test]
fn xval_set_x_0() {
    let reference = pio_reference(
        InstructionOperands::SET {
            destination: SetDestination::X,
            data: 0,
        },
        0,
    );
    assert_eq!(sema_single("(pio/set :x 0)"), reference);
}

#[test]
fn xval_set_x_31() {
    let reference = pio_reference(
        InstructionOperands::SET {
            destination: SetDestination::X,
            data: 31,
        },
        0,
    );
    assert_eq!(sema_single("(pio/set :x 31)"), reference);
}

#[test]
fn xval_set_y_15() {
    let reference = pio_reference(
        InstructionOperands::SET {
            destination: SetDestination::Y,
            data: 15,
        },
        0,
    );
    assert_eq!(sema_single("(pio/set :y 15)"), reference);
}

#[test]
fn xval_set_pindirs_1() {
    let reference = pio_reference(
        InstructionOperands::SET {
            destination: SetDestination::PINDIRS,
            data: 1,
        },
        0,
    );
    assert_eq!(sema_single("(pio/set :pindirs 1)"), reference);
}

// ═══════════════════════════════════════════════════════════════════
// NOP / MOV instructions (opcode 101)
// ═══════════════════════════════════════════════════════════════════

#[test]
fn xval_nop() {
    let reference = pio_reference(
        InstructionOperands::MOV {
            destination: MovDestination::Y,
            op: MovOperation::None,
            source: MovSource::Y,
        },
        0,
    );
    assert_eq!(sema_single("(pio/nop)"), reference);
}

#[test]
fn xval_mov_x_y() {
    let reference = pio_reference(
        InstructionOperands::MOV {
            destination: MovDestination::X,
            op: MovOperation::None,
            source: MovSource::Y,
        },
        0,
    );
    assert_eq!(sema_single("(pio/mov :x :y)"), reference);
}

#[test]
fn xval_mov_y_x() {
    let reference = pio_reference(
        InstructionOperands::MOV {
            destination: MovDestination::Y,
            op: MovOperation::None,
            source: MovSource::X,
        },
        0,
    );
    assert_eq!(sema_single("(pio/mov :y :x)"), reference);
}

#[test]
fn xval_mov_x_invert_y() {
    let reference = pio_reference(
        InstructionOperands::MOV {
            destination: MovDestination::X,
            op: MovOperation::Invert,
            source: MovSource::Y,
        },
        0,
    );
    assert_eq!(sema_single("(pio/mov :x :!y)"), reference);
}

#[test]
fn xval_mov_x_reverse_y() {
    let reference = pio_reference(
        InstructionOperands::MOV {
            destination: MovDestination::X,
            op: MovOperation::BitReverse,
            source: MovSource::Y,
        },
        0,
    );
    assert_eq!(sema_single("(pio/mov :x :y :reverse)"), reference);
}

#[test]
fn xval_mov_pins_isr() {
    let reference = pio_reference(
        InstructionOperands::MOV {
            destination: MovDestination::PINS,
            op: MovOperation::None,
            source: MovSource::ISR,
        },
        0,
    );
    assert_eq!(sema_single("(pio/mov :pins :isr)"), reference);
}

#[test]
fn xval_mov_osr_null() {
    let reference = pio_reference(
        InstructionOperands::MOV {
            destination: MovDestination::OSR,
            op: MovOperation::None,
            source: MovSource::NULL,
        },
        0,
    );
    assert_eq!(sema_single("(pio/mov :osr :null)"), reference);
}

#[test]
fn xval_mov_isr_osr() {
    let reference = pio_reference(
        InstructionOperands::MOV {
            destination: MovDestination::ISR,
            op: MovOperation::None,
            source: MovSource::OSR,
        },
        0,
    );
    assert_eq!(sema_single("(pio/mov :isr :osr)"), reference);
}

#[test]
fn xval_mov_exec_invert_status() {
    let reference = pio_reference(
        InstructionOperands::MOV {
            destination: MovDestination::EXEC,
            op: MovOperation::Invert,
            source: MovSource::STATUS,
        },
        0,
    );
    assert_eq!(sema_single("(pio/mov :exec :!status)"), reference);
}

// ═══════════════════════════════════════════════════════════════════
// IN instructions (opcode 010)
// ═══════════════════════════════════════════════════════════════════

#[test]
fn xval_in_pins_1() {
    let reference = pio_reference(
        InstructionOperands::IN {
            source: InSource::PINS,
            bit_count: 1,
        },
        0,
    );
    assert_eq!(sema_single("(pio/in :pins 1)"), reference);
}

#[test]
fn xval_in_pins_8() {
    let reference = pio_reference(
        InstructionOperands::IN {
            source: InSource::PINS,
            bit_count: 8,
        },
        0,
    );
    assert_eq!(sema_single("(pio/in :pins 8)"), reference);
}

#[test]
fn xval_in_pins_32() {
    let reference = pio_reference(
        InstructionOperands::IN {
            source: InSource::PINS,
            bit_count: 32,
        },
        0,
    );
    assert_eq!(sema_single("(pio/in :pins 32)"), reference);
}

#[test]
fn xval_in_x_32() {
    let reference = pio_reference(
        InstructionOperands::IN {
            source: InSource::X,
            bit_count: 32,
        },
        0,
    );
    assert_eq!(sema_single("(pio/in :x 32)"), reference);
}

#[test]
fn xval_in_null_4() {
    let reference = pio_reference(
        InstructionOperands::IN {
            source: InSource::NULL,
            bit_count: 4,
        },
        0,
    );
    assert_eq!(sema_single("(pio/in :null 4)"), reference);
}

#[test]
fn xval_in_isr_16() {
    let reference = pio_reference(
        InstructionOperands::IN {
            source: InSource::ISR,
            bit_count: 16,
        },
        0,
    );
    assert_eq!(sema_single("(pio/in :isr 16)"), reference);
}

#[test]
fn xval_in_osr_8() {
    let reference = pio_reference(
        InstructionOperands::IN {
            source: InSource::OSR,
            bit_count: 8,
        },
        0,
    );
    assert_eq!(sema_single("(pio/in :osr 8)"), reference);
}

// ═══════════════════════════════════════════════════════════════════
// OUT instructions (opcode 011)
// ═══════════════════════════════════════════════════════════════════

#[test]
fn xval_out_pins_1() {
    let reference = pio_reference(
        InstructionOperands::OUT {
            destination: OutDestination::PINS,
            bit_count: 1,
        },
        0,
    );
    assert_eq!(sema_single("(pio/out :pins 1)"), reference);
}

#[test]
fn xval_out_pins_32() {
    let reference = pio_reference(
        InstructionOperands::OUT {
            destination: OutDestination::PINS,
            bit_count: 32,
        },
        0,
    );
    assert_eq!(sema_single("(pio/out :pins 32)"), reference);
}

#[test]
fn xval_out_x_32() {
    let reference = pio_reference(
        InstructionOperands::OUT {
            destination: OutDestination::X,
            bit_count: 32,
        },
        0,
    );
    assert_eq!(sema_single("(pio/out :x 32)"), reference);
}

#[test]
fn xval_out_y_8() {
    let reference = pio_reference(
        InstructionOperands::OUT {
            destination: OutDestination::Y,
            bit_count: 8,
        },
        0,
    );
    assert_eq!(sema_single("(pio/out :y 8)"), reference);
}

#[test]
fn xval_out_null_1() {
    let reference = pio_reference(
        InstructionOperands::OUT {
            destination: OutDestination::NULL,
            bit_count: 1,
        },
        0,
    );
    assert_eq!(sema_single("(pio/out :null 1)"), reference);
}

#[test]
fn xval_out_pindirs_4() {
    let reference = pio_reference(
        InstructionOperands::OUT {
            destination: OutDestination::PINDIRS,
            bit_count: 4,
        },
        0,
    );
    assert_eq!(sema_single("(pio/out :pindirs 4)"), reference);
}

#[test]
fn xval_out_pc_5() {
    let reference = pio_reference(
        InstructionOperands::OUT {
            destination: OutDestination::PC,
            bit_count: 5,
        },
        0,
    );
    assert_eq!(sema_single("(pio/out :pc 5)"), reference);
}

#[test]
fn xval_out_isr_16() {
    let reference = pio_reference(
        InstructionOperands::OUT {
            destination: OutDestination::ISR,
            bit_count: 16,
        },
        0,
    );
    assert_eq!(sema_single("(pio/out :isr 16)"), reference);
}

#[test]
fn xval_out_exec_16() {
    let reference = pio_reference(
        InstructionOperands::OUT {
            destination: OutDestination::EXEC,
            bit_count: 16,
        },
        0,
    );
    assert_eq!(sema_single("(pio/out :exec 16)"), reference);
}

// ═══════════════════════════════════════════════════════════════════
// PUSH instructions (opcode 100, bit7=0)
// ═══════════════════════════════════════════════════════════════════

#[test]
fn xval_push_block() {
    let reference = pio_reference(
        InstructionOperands::PUSH {
            if_full: false,
            block: true,
        },
        0,
    );
    assert_eq!(sema_single("(pio/push)"), reference);
}

#[test]
fn xval_push_noblock() {
    let reference = pio_reference(
        InstructionOperands::PUSH {
            if_full: false,
            block: false,
        },
        0,
    );
    assert_eq!(sema_single("(pio/push :no-block)"), reference);
}

#[test]
fn xval_push_iffull_block() {
    let reference = pio_reference(
        InstructionOperands::PUSH {
            if_full: true,
            block: true,
        },
        0,
    );
    assert_eq!(sema_single("(pio/push :iffull)"), reference);
}

#[test]
fn xval_push_iffull_noblock() {
    let reference = pio_reference(
        InstructionOperands::PUSH {
            if_full: true,
            block: false,
        },
        0,
    );
    assert_eq!(sema_single("(pio/push :iffull :no-block)"), reference);
}

// ═══════════════════════════════════════════════════════════════════
// PULL instructions (opcode 100, bit7=1)
// ═══════════════════════════════════════════════════════════════════

#[test]
fn xval_pull_block() {
    let reference = pio_reference(
        InstructionOperands::PULL {
            if_empty: false,
            block: true,
        },
        0,
    );
    assert_eq!(sema_single("(pio/pull)"), reference);
}

#[test]
fn xval_pull_noblock() {
    let reference = pio_reference(
        InstructionOperands::PULL {
            if_empty: false,
            block: false,
        },
        0,
    );
    assert_eq!(sema_single("(pio/pull :no-block)"), reference);
}

#[test]
fn xval_pull_ifempty_block() {
    let reference = pio_reference(
        InstructionOperands::PULL {
            if_empty: true,
            block: true,
        },
        0,
    );
    assert_eq!(sema_single("(pio/pull :ifempty)"), reference);
}

#[test]
fn xval_pull_ifempty_noblock() {
    let reference = pio_reference(
        InstructionOperands::PULL {
            if_empty: true,
            block: false,
        },
        0,
    );
    assert_eq!(sema_single("(pio/pull :ifempty :no-block)"), reference);
}

// ═══════════════════════════════════════════════════════════════════
// WAIT instructions (opcode 001)
// ═══════════════════════════════════════════════════════════════════

#[test]
fn xval_wait_gpio_0_high() {
    let reference = pio_reference(
        InstructionOperands::WAIT {
            polarity: 1,
            source: WaitSource::GPIO,
            index: 0,
            relative: false,
        },
        0,
    );
    assert_eq!(sema_single("(pio/wait 1 :gpio 0)"), reference);
}

#[test]
fn xval_wait_gpio_15_low() {
    let reference = pio_reference(
        InstructionOperands::WAIT {
            polarity: 0,
            source: WaitSource::GPIO,
            index: 15,
            relative: false,
        },
        0,
    );
    assert_eq!(sema_single("(pio/wait 0 :gpio 15)"), reference);
}

#[test]
fn xval_wait_pin_0_high() {
    let reference = pio_reference(
        InstructionOperands::WAIT {
            polarity: 1,
            source: WaitSource::PIN,
            index: 0,
            relative: false,
        },
        0,
    );
    assert_eq!(sema_single("(pio/wait 1 :pin 0)"), reference);
}

#[test]
fn xval_wait_irq_3() {
    let reference = pio_reference(
        InstructionOperands::WAIT {
            polarity: 1,
            source: WaitSource::IRQ,
            index: 3,
            relative: false,
        },
        0,
    );
    assert_eq!(sema_single("(pio/wait 1 :irq 3)"), reference);
}

#[test]
fn xval_wait_irq_rel() {
    let reference = pio_reference(
        InstructionOperands::WAIT {
            polarity: 1,
            source: WaitSource::IRQ,
            index: 0,
            relative: true,
        },
        0,
    );
    assert_eq!(sema_single("(pio/wait 1 :irq 0 :rel)"), reference);
}

// ═══════════════════════════════════════════════════════════════════
// IRQ instructions (opcode 110)
// ═══════════════════════════════════════════════════════════════════

#[test]
fn xval_irq_set_0() {
    let reference = pio_reference(
        InstructionOperands::IRQ {
            clear: false,
            wait: false,
            index: 0,
            index_mode: IrqIndexMode::DIRECT,
        },
        0,
    );
    assert_eq!(sema_single("(pio/irq :set 0)"), reference);
}

#[test]
fn xval_irq_set_7() {
    let reference = pio_reference(
        InstructionOperands::IRQ {
            clear: false,
            wait: false,
            index: 7,
            index_mode: IrqIndexMode::DIRECT,
        },
        0,
    );
    assert_eq!(sema_single("(pio/irq :set 7)"), reference);
}

#[test]
fn xval_irq_wait_2() {
    let reference = pio_reference(
        InstructionOperands::IRQ {
            clear: false,
            wait: true,
            index: 2,
            index_mode: IrqIndexMode::DIRECT,
        },
        0,
    );
    assert_eq!(sema_single("(pio/irq :wait 2)"), reference);
}

#[test]
fn xval_irq_clear_5() {
    let reference = pio_reference(
        InstructionOperands::IRQ {
            clear: true,
            wait: false,
            index: 5,
            index_mode: IrqIndexMode::DIRECT,
        },
        0,
    );
    assert_eq!(sema_single("(pio/irq :clear 5)"), reference);
}

#[test]
fn xval_irq_set_rel() {
    let reference = pio_reference(
        InstructionOperands::IRQ {
            clear: false,
            wait: false,
            index: 0,
            index_mode: IrqIndexMode::REL,
        },
        0,
    );
    assert_eq!(sema_single("(pio/irq :set 0 :rel)"), reference);
}

#[test]
fn xval_irq_wait_rel() {
    let reference = pio_reference(
        InstructionOperands::IRQ {
            clear: false,
            wait: true,
            index: 3,
            index_mode: IrqIndexMode::REL,
        },
        0,
    );
    assert_eq!(sema_single("(pio/irq :wait 3 :rel)"), reference);
}

// ═══════════════════════════════════════════════════════════════════
// JMP instructions (opcode 000) — all 8 conditions
// ═══════════════════════════════════════════════════════════════════

#[test]
fn xval_jmp_always() {
    let reference = pio_assemble_reference(|a| {
        let mut label = a.label();
        a.bind(&mut label);
        a.jmp(JmpCondition::Always, &mut label);
    });
    assert_eq!(sema_assemble("(list 'target (pio/jmp 'target))"), reference);
}

#[test]
fn xval_jmp_not_x() {
    let reference = pio_assemble_reference(|a| {
        let mut label = a.label();
        a.bind(&mut label);
        a.jmp(JmpCondition::XIsZero, &mut label);
    });
    assert_eq!(
        sema_assemble("(list 'target (pio/jmp :!x 'target))"),
        reference
    );
}

#[test]
fn xval_jmp_x_dec() {
    let reference = pio_assemble_reference(|a| {
        let mut label = a.label();
        a.bind(&mut label);
        a.jmp(JmpCondition::XDecNonZero, &mut label);
    });
    assert_eq!(
        sema_assemble("(list 'target (pio/jmp :x-- 'target))"),
        reference
    );
}

#[test]
fn xval_jmp_not_y() {
    let reference = pio_assemble_reference(|a| {
        let mut label = a.label();
        a.bind(&mut label);
        a.jmp(JmpCondition::YIsZero, &mut label);
    });
    assert_eq!(
        sema_assemble("(list 'target (pio/jmp :!y 'target))"),
        reference
    );
}

#[test]
fn xval_jmp_y_dec() {
    let reference = pio_assemble_reference(|a| {
        let mut label = a.label();
        a.bind(&mut label);
        a.jmp(JmpCondition::YDecNonZero, &mut label);
    });
    assert_eq!(
        sema_assemble("(list 'target (pio/jmp :y-- 'target))"),
        reference
    );
}

#[test]
fn xval_jmp_x_ne_y() {
    let reference = pio_assemble_reference(|a| {
        let mut label = a.label();
        a.bind(&mut label);
        a.jmp(JmpCondition::XNotEqualY, &mut label);
    });
    assert_eq!(
        sema_assemble("(list 'target (pio/jmp :x!=y 'target))"),
        reference
    );
}

#[test]
fn xval_jmp_pin() {
    let reference = pio_assemble_reference(|a| {
        let mut label = a.label();
        a.bind(&mut label);
        a.jmp(JmpCondition::PinHigh, &mut label);
    });
    assert_eq!(
        sema_assemble("(list 'target (pio/jmp :pin 'target))"),
        reference
    );
}

#[test]
fn xval_jmp_not_osre() {
    let reference = pio_assemble_reference(|a| {
        let mut label = a.label();
        a.bind(&mut label);
        a.jmp(JmpCondition::OutputShiftRegisterNotEmpty, &mut label);
    });
    assert_eq!(
        sema_assemble("(list 'target (pio/jmp :!osre 'target))"),
        reference
    );
}

// ═══════════════════════════════════════════════════════════════════
// Delay encoding
// ═══════════════════════════════════════════════════════════════════

#[test]
fn xval_nop_delay_1() {
    let reference = pio_reference(
        InstructionOperands::MOV {
            destination: MovDestination::Y,
            op: MovOperation::None,
            source: MovSource::Y,
        },
        1,
    );
    assert_eq!(sema_single("(pio/delay 1 (pio/nop))"), reference);
}

#[test]
fn xval_nop_delay_15() {
    let reference = pio_reference(
        InstructionOperands::MOV {
            destination: MovDestination::Y,
            op: MovOperation::None,
            source: MovSource::Y,
        },
        15,
    );
    assert_eq!(sema_single("(pio/delay 15 (pio/nop))"), reference);
}

#[test]
fn xval_nop_delay_31() {
    let reference = pio_reference(
        InstructionOperands::MOV {
            destination: MovDestination::Y,
            op: MovOperation::None,
            source: MovSource::Y,
        },
        31,
    );
    assert_eq!(sema_single("(pio/delay 31 (pio/nop))"), reference);
}

#[test]
fn xval_set_delay_7() {
    let reference = pio_reference(
        InstructionOperands::SET {
            destination: SetDestination::PINS,
            data: 1,
        },
        7,
    );
    assert_eq!(sema_single("(pio/delay 7 (pio/set :pins 1))"), reference);
}

// ═══════════════════════════════════════════════════════════════════
// Multi-instruction programs with labels
// ═══════════════════════════════════════════════════════════════════

#[test]
fn xval_hello_pio() {
    // The canonical hello.pio from pico-examples:
    //   pull block
    //   out pins, 1
    //   jmp start
    let reference = pio_assemble_reference(|a| {
        let mut start = a.label();
        a.bind(&mut start);
        a.pull(false, true);
        a.out(OutDestination::PINS, 1);
        a.jmp(JmpCondition::Always, &mut start);
    });
    assert_eq!(
        sema_assemble("(list 'start (pio/pull) (pio/out :pins 1) (pio/jmp 'start))"),
        reference
    );
}

#[test]
fn xval_blink_program() {
    // Simple blink: set pins 1, nop[31], set pins 0, nop[31]
    let reference = pio_assemble_reference(|a| {
        a.set(SetDestination::PINS, 1);
        a.nop_with_delay(31);
        a.set(SetDestination::PINS, 0);
        a.nop_with_delay(31);
    });
    assert_eq!(
        sema_assemble(
            "(list (pio/set :pins 1) (pio/delay 31 (pio/nop)) (pio/set :pins 0) (pio/delay 31 (pio/nop)))"
        ),
        reference
    );
}

#[test]
fn xval_forward_jump() {
    // jmp end, nop, nop (end label)
    let reference = pio_assemble_reference(|a| {
        let mut end = a.label();
        a.jmp(JmpCondition::Always, &mut end);
        a.nop();
        a.bind(&mut end);
        a.nop();
    });
    assert_eq!(
        sema_assemble("(list (pio/jmp 'end) (pio/nop) 'end (pio/nop))"),
        reference
    );
}

#[test]
fn xval_count_down_loop() {
    // set x 10, loop: jmp x-- loop, nop
    let reference = pio_assemble_reference(|a| {
        let mut lp = a.label();
        a.set(SetDestination::X, 10);
        a.bind(&mut lp);
        a.jmp(JmpCondition::XDecNonZero, &mut lp);
        a.nop();
    });
    assert_eq!(
        sema_assemble("(list (pio/set :x 10) 'loop (pio/jmp :x-- 'loop) (pio/nop))"),
        reference
    );
}

#[test]
fn xval_pull_out_loop() {
    // Typical data output pattern: pull, out pins 8, jmp start
    let reference = pio_assemble_reference(|a| {
        let mut start = a.label();
        a.bind(&mut start);
        a.pull(false, true);
        a.out(OutDestination::PINS, 8);
        a.jmp(JmpCondition::Always, &mut start);
    });
    assert_eq!(
        sema_assemble("(list 'start (pio/pull) (pio/out :pins 8) (pio/jmp 'start))"),
        reference
    );
}

#[test]
fn xval_in_push_loop() {
    // Typical data input pattern: in pins 8, push, jmp start
    let reference = pio_assemble_reference(|a| {
        let mut start = a.label();
        a.bind(&mut start);
        a.r#in(InSource::PINS, 8);
        a.push(false, true);
        a.jmp(JmpCondition::Always, &mut start);
    });
    assert_eq!(
        sema_assemble("(list 'start (pio/in :pins 8) (pio/push) (pio/jmp 'start))"),
        reference
    );
}

// ═══════════════════════════════════════════════════════════════════
// Side-set encoding
// ═══════════════════════════════════════════════════════════════════

#[test]
fn xval_side_set_1bit() {
    // set pins 0 with side-set 1 (1 bit)
    let ss = SideSet::new(false, 1, false);
    let instr = pio::Instruction {
        operands: InstructionOperands::SET {
            destination: SetDestination::PINS,
            data: 0,
        },
        delay: 0,
        side_set: Some(1),
    };
    let word = instr.encode(ss);
    let reference = vec![(word & 0xFF) as u8, (word >> 8) as u8];
    assert_eq!(
        sema_assemble("(list (pio/side 1 (pio/set :pins 0))) {:side-set-bits 1}"),
        reference
    );
}

#[test]
fn xval_side_set_2bit() {
    // nop with side-set 3 (2 bits)
    let ss = SideSet::new(false, 2, false);
    let instr = pio::Instruction {
        operands: InstructionOperands::MOV {
            destination: MovDestination::Y,
            op: MovOperation::None,
            source: MovSource::Y,
        },
        delay: 0,
        side_set: Some(3),
    };
    let word = instr.encode(ss);
    let reference = vec![(word & 0xFF) as u8, (word >> 8) as u8];
    assert_eq!(
        sema_assemble("(list (pio/side 3 (pio/nop))) {:side-set-bits 2}"),
        reference
    );
}

#[test]
fn xval_side_set_with_delay() {
    // set pins 1, side-set 1, delay 3 (1 side-set bit, 4 delay bits)
    let ss = SideSet::new(false, 1, false);
    let instr = pio::Instruction {
        operands: InstructionOperands::SET {
            destination: SetDestination::PINS,
            data: 1,
        },
        delay: 3,
        side_set: Some(1),
    };
    let word = instr.encode(ss);
    let reference = vec![(word & 0xFF) as u8, (word >> 8) as u8];
    assert_eq!(
        sema_assemble("(list (pio/side 1 (pio/delay 3 (pio/set :pins 1)))) {:side-set-bits 1}"),
        reference
    );
}
