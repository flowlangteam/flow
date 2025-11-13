// Java bytecode instructions and constants

// Access flags for classes
pub const ACC_PUBLIC: u16 = 0x0001;
pub const ACC_PRIVATE: u16 = 0x0002;
pub const ACC_PROTECTED: u16 = 0x0004;
pub const ACC_STATIC: u16 = 0x0008;
pub const ACC_FINAL: u16 = 0x0010;
pub const ACC_SUPER: u16 = 0x0020;
pub const ACC_INTERFACE: u16 = 0x0200;
pub const ACC_ABSTRACT: u16 = 0x0400;

// Java bytecode opcodes
pub const ICONST_M1: u8 = 0x02;
pub const ICONST_0: u8 = 0x03;
pub const ICONST_1: u8 = 0x04;
pub const ICONST_2: u8 = 0x05;
pub const ICONST_3: u8 = 0x06;
pub const ICONST_4: u8 = 0x07;
pub const ICONST_5: u8 = 0x08;

pub const LCONST_0: u8 = 0x09;
pub const LCONST_1: u8 = 0x0A;

pub const FCONST_0: u8 = 0x0B;
pub const FCONST_1: u8 = 0x0C;
pub const FCONST_2: u8 = 0x0D;

pub const DCONST_0: u8 = 0x0E;
pub const DCONST_1: u8 = 0x0F;

pub const BIPUSH: u8 = 0x10;
pub const SIPUSH: u8 = 0x11;
pub const LDC: u8 = 0x12;
pub const LDC_W: u8 = 0x13;
pub const LDC2_W: u8 = 0x14;

pub const ILOAD: u8 = 0x15;
pub const LLOAD: u8 = 0x16;
pub const FLOAD: u8 = 0x17;
pub const DLOAD: u8 = 0x18;
pub const ALOAD: u8 = 0x19;

pub const ILOAD_0: u8 = 0x1A;
pub const ILOAD_1: u8 = 0x1B;
pub const ILOAD_2: u8 = 0x1C;
pub const ILOAD_3: u8 = 0x1D;

pub const LLOAD_0: u8 = 0x1E;
pub const LLOAD_1: u8 = 0x1F;
pub const LLOAD_2: u8 = 0x20;
pub const LLOAD_3: u8 = 0x21;

pub const ISTORE: u8 = 0x36;
pub const LSTORE: u8 = 0x37;
pub const FSTORE: u8 = 0x38;
pub const DSTORE: u8 = 0x39;
pub const ASTORE: u8 = 0x3A;

pub const IADD: u8 = 0x60;
pub const LADD: u8 = 0x61;
pub const FADD: u8 = 0x62;
pub const DADD: u8 = 0x63;

pub const ISUB: u8 = 0x64;
pub const LSUB: u8 = 0x65;
pub const FSUB: u8 = 0x66;
pub const DSUB: u8 = 0x67;

pub const IMUL: u8 = 0x68;
pub const LMUL: u8 = 0x69;
pub const FMUL: u8 = 0x6A;
pub const DMUL: u8 = 0x6B;

pub const IDIV: u8 = 0x6C;
pub const LDIV: u8 = 0x6D;
pub const FDIV: u8 = 0x6E;
pub const DDIV: u8 = 0x6F;

pub const IREM: u8 = 0x70;
pub const LREM: u8 = 0x71;
pub const FREM: u8 = 0x72;
pub const DREM: u8 = 0x73;

pub const INEG: u8 = 0x74;
pub const LNEG: u8 = 0x75;
pub const FNEG: u8 = 0x76;
pub const DNEG: u8 = 0x77;

pub const IAND: u8 = 0x7E;
pub const LAND: u8 = 0x7F;
pub const IOR: u8 = 0x80;
pub const LOR: u8 = 0x81;

pub const LCMP: u8 = 0x94;

pub const IFEQ: u8 = 0x99;
pub const IFNE: u8 = 0x9A;
pub const IFLT: u8 = 0x9B;
pub const IFGE: u8 = 0x9C;
pub const IFGT: u8 = 0x9D;
pub const IFLE: u8 = 0x9E;

pub const IF_ICMPEQ: u8 = 0x9F;
pub const IF_ICMPNE: u8 = 0xA0;
pub const IF_ICMPLT: u8 = 0xA1;
pub const IF_ICMPGE: u8 = 0xA2;
pub const IF_ICMPGT: u8 = 0xA3;
pub const IF_ICMPLE: u8 = 0xA4;

pub const GOTO: u8 = 0xA7;

pub const IRETURN: u8 = 0xAC;
pub const LRETURN: u8 = 0xAD;
pub const FRETURN: u8 = 0xAE;
pub const DRETURN: u8 = 0xAF;
pub const ARETURN: u8 = 0xB0;
pub const RETURN: u8 = 0xB1;

pub const GETSTATIC: u8 = 0xB2;
pub const PUTSTATIC: u8 = 0xB3;
pub const GETFIELD: u8 = 0xB4;
pub const PUTFIELD: u8 = 0xB5;

pub const INVOKEVIRTUAL: u8 = 0xB6;
pub const INVOKESPECIAL: u8 = 0xB7;
pub const INVOKESTATIC: u8 = 0xB8;
pub const INVOKEINTERFACE: u8 = 0xB9;

pub const NEW: u8 = 0xBB;
pub const DUP: u8 = 0x59;

#[derive(Debug, Clone)]
pub enum ConstantValue {
    Integer(i32),
    Long(i64),
    Float(f32),
    Double(f64),
    String(String),
}

#[derive(Debug, Clone)]
pub enum Instruction {
    // Constants
    IConst(i32),
    LConst(i64),
    FConst(f32),
    DConst(f64),
    Ldc(ConstantValue),

    // Loads
    ILoad(u16),
    LLoad(u16),
    FLoad(u16),
    DLoad(u16),
    ALoad(u16),

    // Stores
    IStore(u16),
    LStore(u16),
    FStore(u16),
    DStore(u16),
    AStore(u16),

    // Arithmetic
    IAdd,
    LAdd,
    FAdd,
    DAdd,
    ISub,
    LSub,
    FSub,
    DSub,
    IMul,
    LMul,
    FMul,
    DMul,
    IDiv,
    LDiv,
    FDiv,
    DDiv,
    IRem,
    LRem,
    FRem,
    DRem,
    INeg,
    LNeg,
    FNeg,
    DNeg,

    // Bitwise
    IAnd,
    LAnd,
    IOr,
    LOr,

    // Comparisons
    LCmp,

    // Conditional branches
    IfEq(i16),
    IfNe(i16),
    IfLt(i16),
    IfGe(i16),
    IfGt(i16),
    IfLe(i16),
    IfICmpEq(i16),
    IfICmpNe(i16),
    IfICmpLt(i16),
    IfICmpGe(i16),
    IfICmpGt(i16),
    IfICmpLe(i16),

    // Unconditional branch
    Goto(i16),

    // Returns
    IReturn,
    LReturn,
    FReturn,
    DReturn,
    AReturn,
    Return,

    // Method invocation
    InvokeVirtual(String, String, String), // class, method, descriptor
    InvokeSpecial(String, String, String),
    InvokeStatic(String, String, String),

    // Field access
    GetStatic(String, String, String), // class, field, descriptor
    PutStatic(String, String, String),
    GetField(String, String, String),
    PutField(String, String, String),

    // Object creation
    New(String),
    Dup,
}

pub struct CodeBuilder {
    instructions: Vec<Instruction>,
    offsets: Vec<usize>,
}

impl CodeBuilder {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
            offsets: Vec::new(),
        }
    }

    pub fn add_instruction(&mut self, instr: Instruction) {
        self.offsets.push(self.current_offset());
        self.instructions.push(instr);
    }

    pub fn instruction_count(&self) -> usize {
        self.instructions.len()
    }

    pub fn current_offset(&self) -> usize {
        let mut offset = 0;
        for instr in &self.instructions {
            offset += instruction_size(instr);
        }
        offset
    }

    pub fn patch_jump(&mut self, index: usize, target_offset: i16) {
        if let Some(instr) = self.instructions.get_mut(index) {
            match instr {
                Instruction::IfEq(offset)
                | Instruction::IfNe(offset)
                | Instruction::IfLt(offset)
                | Instruction::IfGe(offset)
                | Instruction::IfGt(offset)
                | Instruction::IfLe(offset)
                | Instruction::IfICmpEq(offset)
                | Instruction::IfICmpNe(offset)
                | Instruction::IfICmpLt(offset)
                | Instruction::IfICmpGe(offset)
                | Instruction::IfICmpGt(offset)
                | Instruction::IfICmpLe(offset)
                | Instruction::Goto(offset) => {
                    *offset = target_offset;
                }
                _ => {}
            }
        }
    }

    pub fn build(self) -> Vec<Instruction> {
        self.instructions
    }
}

fn instruction_size(instr: &Instruction) -> usize {
    match instr {
        Instruction::IConst(_) => 2,
        Instruction::LConst(_) => 1,
        Instruction::Ldc(_) => 2,
        Instruction::ILoad(_)
        | Instruction::LLoad(_)
        | Instruction::FLoad(_)
        | Instruction::DLoad(_)
        | Instruction::ALoad(_) => 2,
        Instruction::IStore(_)
        | Instruction::LStore(_)
        | Instruction::FStore(_)
        | Instruction::DStore(_)
        | Instruction::AStore(_) => 2,
        Instruction::IAdd
        | Instruction::LAdd
        | Instruction::FAdd
        | Instruction::DAdd
        | Instruction::ISub
        | Instruction::LSub
        | Instruction::FSub
        | Instruction::DSub
        | Instruction::IMul
        | Instruction::LMul
        | Instruction::FMul
        | Instruction::DMul
        | Instruction::IDiv
        | Instruction::LDiv
        | Instruction::FDiv
        | Instruction::DDiv
        | Instruction::IRem
        | Instruction::LRem
        | Instruction::FRem
        | Instruction::DRem
        | Instruction::INeg
        | Instruction::LNeg
        | Instruction::FNeg
        | Instruction::DNeg
        | Instruction::IAnd
        | Instruction::LAnd
        | Instruction::IOr
        | Instruction::LOr => 1,
        Instruction::LCmp => 1,
        Instruction::IfEq(_)
        | Instruction::IfNe(_)
        | Instruction::IfLt(_)
        | Instruction::IfGe(_)
        | Instruction::IfGt(_)
        | Instruction::IfLe(_)
        | Instruction::IfICmpEq(_)
        | Instruction::IfICmpNe(_)
        | Instruction::IfICmpLt(_)
        | Instruction::IfICmpGe(_)
        | Instruction::IfICmpGt(_)
        | Instruction::IfICmpLe(_) => 3,
        Instruction::Goto(_) => 3,
        Instruction::IReturn
        | Instruction::LReturn
        | Instruction::FReturn
        | Instruction::DReturn
        | Instruction::AReturn
        | Instruction::Return => 1,
        Instruction::InvokeVirtual(_, _, _)
        | Instruction::InvokeSpecial(_, _, _)
        | Instruction::InvokeStatic(_, _, _) => 3,
        Instruction::GetStatic(_, _, _)
        | Instruction::PutStatic(_, _, _)
        | Instruction::GetField(_, _, _)
        | Instruction::PutField(_, _, _) => 3,
        Instruction::New(_) => 3,
        Instruction::Dup => 1,
        _ => 1,
    }
}
