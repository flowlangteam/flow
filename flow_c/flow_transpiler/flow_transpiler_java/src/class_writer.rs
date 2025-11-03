use crate::constant_pool::ConstantPool;
use crate::bytecode::*;
use byteorder::{BigEndian, WriteBytesExt};
use flow_transpiler::Result;
use std::io::Write;

const JAVA_VERSION: u16 = 52; // Java 8

pub struct ClassWriter {
    constant_pool: ConstantPool,
    class_name: String,
    super_class: String,
    access_flags: u16,
    interfaces: Vec<String>,
    fields: Vec<FieldInfo>,
    methods: Vec<MethodInfo>,
}

pub struct FieldInfo {
    access_flags: u16,
    name: String,
    descriptor: String,
}

pub struct MethodInfo {
    access_flags: u16,
    name: String,
    descriptor: String,
    code: Option<Vec<Instruction>>,
    max_stack: u16,
    max_locals: u16,
}

impl Clone for MethodInfo {
    fn clone(&self) -> Self {
        Self {
            access_flags: self.access_flags,
            name: self.name.clone(),
            descriptor: self.descriptor.clone(),
            code: self.code.clone(),
            max_stack: self.max_stack,
            max_locals: self.max_locals,
        }
    }
}

impl ClassWriter {
    pub fn new(class_name: impl Into<String>) -> Self {
        Self {
            constant_pool: ConstantPool::new(),
            class_name: class_name.into(),
            super_class: "java/lang/Object".to_string(),
            access_flags: ACC_PUBLIC | ACC_SUPER,
            interfaces: Vec::new(),
            fields: Vec::new(),
            methods: Vec::new(),
        }
    }
    
    pub fn set_super_class(&mut self, super_class: impl Into<String>) {
        self.super_class = super_class.into();
    }
    
    pub fn set_access_flags(&mut self, flags: u16) {
        self.access_flags = flags;
    }
    
    pub fn add_interface(&mut self, interface: impl Into<String>) {
        self.interfaces.push(interface.into());
    }
    
    pub fn add_field(&mut self, name: impl Into<String>, descriptor: impl Into<String>, access_flags: u16) {
        self.fields.push(FieldInfo {
            access_flags,
            name: name.into(),
            descriptor: descriptor.into(),
        });
    }
    
    pub fn add_method(&mut self, name: impl Into<String>) -> MethodBuilder {
        MethodBuilder {
            class: self,
            name: name.into(),
            descriptor: String::new(),
            access_flags: 0,
            code: None,
            max_stack: 100,
            max_locals: 100,
        }
    }
    
    pub fn add_default_constructor(&mut self) {
        let mut constructor = self.add_method("<init>");
        constructor.set_descriptor("()V");
        constructor.set_access_flags(ACC_PUBLIC);
        
        let mut code = CodeBuilder::new();
        code.add_instruction(Instruction::ALoad(0)); // Load 'this'
        code.add_instruction(Instruction::InvokeSpecial(
            "java/lang/Object".to_string(),
            "<init>".to_string(),
            "()V".to_string(),
        ));
        code.add_instruction(Instruction::Return);
        
        constructor.set_code(code.build());
    }
    
    pub fn write_bytes(&mut self) -> Result<Vec<u8>> {
        let mut bytes = Vec::new();
        
        // Magic number
        bytes.write_u32::<BigEndian>(0xCAFEBABE)?;
        
        // Minor and major version
        bytes.write_u16::<BigEndian>(0)?; // minor
        bytes.write_u16::<BigEndian>(JAVA_VERSION)?; // major
        
        // Build constant pool with all necessary entries
        self.build_constant_pool();
        
        // Write constant pool
        self.constant_pool.write(&mut bytes)?;
        
        // Access flags
        bytes.write_u16::<BigEndian>(self.access_flags)?;
        
        // This class (already added in build_constant_pool)
        let this_class_index = self.constant_pool.get_class_index(&self.class_name);
        bytes.write_u16::<BigEndian>(this_class_index)?;
        
        // Super class (already added in build_constant_pool)
        let super_class_index = self.constant_pool.get_class_index(&self.super_class);
        bytes.write_u16::<BigEndian>(super_class_index)?;
        
        // Interfaces
        bytes.write_u16::<BigEndian>(self.interfaces.len() as u16)?;
        for interface in &self.interfaces {
            let interface_index = self.constant_pool.get_class_index(interface);
            bytes.write_u16::<BigEndian>(interface_index)?;
        }
        
        // Fields
        bytes.write_u16::<BigEndian>(self.fields.len() as u16)?;
        for field in &self.fields {
            bytes.write_u16::<BigEndian>(field.access_flags)?;
            let name_index = self.constant_pool.get_utf8_index(&field.name);
            bytes.write_u16::<BigEndian>(name_index)?;
            let desc_index = self.constant_pool.get_utf8_index(&field.descriptor);
            bytes.write_u16::<BigEndian>(desc_index)?;
            bytes.write_u16::<BigEndian>(0)?; // attributes count
        }
        
        // Methods
        bytes.write_u16::<BigEndian>(self.methods.len() as u16)?;
        let methods_clone = self.methods.clone();
        for method in &methods_clone {
            self.write_method(&mut bytes, method)?;
        }
        
        // Class attributes
        bytes.write_u16::<BigEndian>(0)?; // attributes count
        
        Ok(bytes)
    }
    
    fn build_constant_pool(&mut self) {
        // Add class names
        self.constant_pool.add_class(&self.class_name);
        self.constant_pool.add_class(&self.super_class);
        
        for interface in &self.interfaces {
            self.constant_pool.add_class(interface);
        }
        
        // Add field names and descriptors
        for field in &self.fields {
            self.constant_pool.add_utf8(&field.name);
            self.constant_pool.add_utf8(&field.descriptor);
        }
        
        // Add method names, descriptors, and process code
        for i in 0..self.methods.len() {
            let method = &self.methods[i];
            self.constant_pool.add_utf8(&method.name);
            self.constant_pool.add_utf8(&method.descriptor);
            self.constant_pool.add_utf8("Code");
            
            if let Some(code) = &method.code {
                let code_clone = code.clone();
                self.add_code_constants(&code_clone);
            }
        }
    }
    
    fn add_code_constants(&mut self, code: &[Instruction]) {
        for instr in code {
            match instr {
                Instruction::Ldc(ConstantValue::Integer(v)) => {
                    self.constant_pool.add_integer(*v);
                }
                Instruction::Ldc(ConstantValue::Long(v)) => {
                    self.constant_pool.add_long(*v);
                }
                Instruction::Ldc(ConstantValue::Float(v)) => {
                    self.constant_pool.add_float(*v);
                }
                Instruction::Ldc(ConstantValue::Double(v)) => {
                    self.constant_pool.add_double(*v);
                }
                Instruction::Ldc(ConstantValue::String(s)) => {
                    self.constant_pool.add_string(s);
                }
                Instruction::InvokeVirtual(class, method, desc)
                | Instruction::InvokeSpecial(class, method, desc)
                | Instruction::InvokeStatic(class, method, desc) => {
                    self.constant_pool.add_methodref(class, method, desc);
                }
                Instruction::GetStatic(class, field, desc)
                | Instruction::PutStatic(class, field, desc)
                | Instruction::GetField(class, field, desc)
                | Instruction::PutField(class, field, desc) => {
                    self.constant_pool.add_fieldref(class, field, desc);
                }
                Instruction::New(class) => {
                    self.constant_pool.add_class(class);
                }
                _ => {}
            }
        }
    }
    
    fn write_method(&mut self, bytes: &mut Vec<u8>, method: &MethodInfo) -> Result<()> {
        bytes.write_u16::<BigEndian>(method.access_flags)?;
        
        let name_index = self.constant_pool.get_utf8_index(&method.name);
        bytes.write_u16::<BigEndian>(name_index)?;
        
        let desc_index = self.constant_pool.get_utf8_index(&method.descriptor);
        bytes.write_u16::<BigEndian>(desc_index)?;
        
        // Attributes
        if let Some(code) = &method.code {
            bytes.write_u16::<BigEndian>(1)?; // One attribute (Code)
            self.write_code_attribute(bytes, method, code)?;
        } else {
            bytes.write_u16::<BigEndian>(0)?; // No attributes
        }
        
        Ok(())
    }
    
    fn write_code_attribute(&mut self, bytes: &mut Vec<u8>, method: &MethodInfo, code: &[Instruction]) -> Result<()> {
        let code_name_index = self.constant_pool.get_utf8_index("Code");
        bytes.write_u16::<BigEndian>(code_name_index)?;
        
        // Write code to temporary buffer to calculate length
        let mut code_bytes = Vec::new();
        self.write_instructions(&mut code_bytes, code)?;
        
        // Attribute length = max_stack(2) + max_locals(2) + code_length(4) + code + exception_table_length(2) + attributes_count(2)
        let attribute_length = 2 + 2 + 4 + code_bytes.len() + 2 + 2;
        bytes.write_u32::<BigEndian>(attribute_length as u32)?;
        
        // Max stack and max locals
        bytes.write_u16::<BigEndian>(method.max_stack)?;
        bytes.write_u16::<BigEndian>(method.max_locals)?;
        
        // Code length and code
        bytes.write_u32::<BigEndian>(code_bytes.len() as u32)?;
        bytes.write_all(&code_bytes)?;
        
        // Exception table (empty)
        bytes.write_u16::<BigEndian>(0)?;
        
        // Code attributes (empty)
        bytes.write_u16::<BigEndian>(0)?;
        
        Ok(())
    }
    
    fn write_instructions(&mut self, bytes: &mut Vec<u8>, code: &[Instruction]) -> Result<()> {
        for instr in code {
            match instr {
                Instruction::IConst(n) => {
                    match n {
                        -1 => bytes.write_u8(ICONST_M1)?,
                        0 => bytes.write_u8(ICONST_0)?,
                        1 => bytes.write_u8(ICONST_1)?,
                        2 => bytes.write_u8(ICONST_2)?,
                        3 => bytes.write_u8(ICONST_3)?,
                        4 => bytes.write_u8(ICONST_4)?,
                        5 => bytes.write_u8(ICONST_5)?,
                        -128..=127 => {
                            bytes.write_u8(BIPUSH)?;
                            bytes.write_i8(*n as i8)?;
                        }
                        -32768..=32767 => {
                            bytes.write_u8(SIPUSH)?;
                            bytes.write_i16::<BigEndian>(*n as i16)?;
                        }
                        _ => {
                            let index = self.constant_pool.add_integer(*n);
                            bytes.write_u8(LDC_W)?;
                            bytes.write_u16::<BigEndian>(index)?;
                        }
                    }
                }
                Instruction::LConst(n) => {
                    match n {
                        0 => bytes.write_u8(LCONST_0)?,
                        1 => bytes.write_u8(LCONST_1)?,
                        _ => {
                            let index = self.constant_pool.add_long(*n);
                            bytes.write_u8(LDC2_W)?;
                            bytes.write_u16::<BigEndian>(index)?;
                        }
                    }
                }
                Instruction::FConst(f) => {
                    if *f == 0.0 {
                        bytes.write_u8(FCONST_0)?;
                    } else if *f == 1.0 {
                        bytes.write_u8(FCONST_1)?;
                    } else if *f == 2.0 {
                        bytes.write_u8(FCONST_2)?;
                    } else {
                        let index = self.constant_pool.add_float(*f);
                        bytes.write_u8(LDC_W)?;
                        bytes.write_u16::<BigEndian>(index)?;
                    }
                }
                Instruction::DConst(d) => {
                    if *d == 0.0 {
                        bytes.write_u8(DCONST_0)?;
                    } else if *d == 1.0 {
                        bytes.write_u8(DCONST_1)?;
                    } else {
                        let index = self.constant_pool.add_double(*d);
                        bytes.write_u8(LDC2_W)?;
                        bytes.write_u16::<BigEndian>(index)?;
                    }
                }
                Instruction::Ldc(value) => {
                    let index = match value {
                        ConstantValue::Integer(v) => self.constant_pool.add_integer(*v),
                        ConstantValue::Float(v) => self.constant_pool.add_float(*v),
                        ConstantValue::String(s) => self.constant_pool.add_string(s),
                        ConstantValue::Long(v) => {
                            let idx = self.constant_pool.add_long(*v);
                            bytes.write_u8(LDC2_W)?;
                            bytes.write_u16::<BigEndian>(idx)?;
                            continue;
                        }
                        ConstantValue::Double(v) => {
                            let idx = self.constant_pool.add_double(*v);
                            bytes.write_u8(LDC2_W)?;
                            bytes.write_u16::<BigEndian>(idx)?;
                            continue;
                        }
                    };
                    if index > 255 {
                        bytes.write_u8(LDC_W)?;
                        bytes.write_u16::<BigEndian>(index)?;
                    } else {
                        bytes.write_u8(LDC)?;
                        bytes.write_u8(index as u8)?;
                    }
                }
                Instruction::ILoad(index) => {
                    match index {
                        0 => bytes.write_u8(ILOAD_0)?,
                        1 => bytes.write_u8(ILOAD_1)?,
                        2 => bytes.write_u8(ILOAD_2)?,
                        3 => bytes.write_u8(ILOAD_3)?,
                        _ => {
                            bytes.write_u8(ILOAD)?;
                            bytes.write_u8(*index as u8)?;
                        }
                    }
                }
                Instruction::LLoad(index) => {
                    match index {
                        0 => bytes.write_u8(LLOAD_0)?,
                        1 => bytes.write_u8(LLOAD_1)?,
                        2 => bytes.write_u8(LLOAD_2)?,
                        3 => bytes.write_u8(LLOAD_3)?,
                        _ => {
                            bytes.write_u8(LLOAD)?;
                            bytes.write_u8(*index as u8)?;
                        }
                    }
                }
                Instruction::FLoad(index) => {
                    bytes.write_u8(FLOAD)?;
                    bytes.write_u8(*index as u8)?;
                }
                Instruction::DLoad(index) => {
                    bytes.write_u8(DLOAD)?;
                    bytes.write_u8(*index as u8)?;
                }
                Instruction::ALoad(index) => {
                    bytes.write_u8(ALOAD)?;
                    bytes.write_u8(*index as u8)?;
                }
                Instruction::IStore(index) => {
                    bytes.write_u8(ISTORE)?;
                    bytes.write_u8(*index as u8)?;
                }
                Instruction::LStore(index) => {
                    bytes.write_u8(LSTORE)?;
                    bytes.write_u8(*index as u8)?;
                }
                Instruction::FStore(index) => {
                    bytes.write_u8(FSTORE)?;
                    bytes.write_u8(*index as u8)?;
                }
                Instruction::DStore(index) => {
                    bytes.write_u8(DSTORE)?;
                    bytes.write_u8(*index as u8)?;
                }
                Instruction::AStore(index) => {
                    bytes.write_u8(ASTORE)?;
                    bytes.write_u8(*index as u8)?;
                }
                Instruction::IAdd => bytes.write_u8(IADD)?,
                Instruction::LAdd => bytes.write_u8(LADD)?,
                Instruction::FAdd => bytes.write_u8(FADD)?,
                Instruction::DAdd => bytes.write_u8(DADD)?,
                Instruction::ISub => bytes.write_u8(ISUB)?,
                Instruction::LSub => bytes.write_u8(LSUB)?,
                Instruction::FSub => bytes.write_u8(FSUB)?,
                Instruction::DSub => bytes.write_u8(DSUB)?,
                Instruction::IMul => bytes.write_u8(IMUL)?,
                Instruction::LMul => bytes.write_u8(LMUL)?,
                Instruction::FMul => bytes.write_u8(FMUL)?,
                Instruction::DMul => bytes.write_u8(DMUL)?,
                Instruction::IDiv => bytes.write_u8(IDIV)?,
                Instruction::LDiv => bytes.write_u8(LDIV)?,
                Instruction::FDiv => bytes.write_u8(FDIV)?,
                Instruction::DDiv => bytes.write_u8(DDIV)?,
                Instruction::IRem => bytes.write_u8(IREM)?,
                Instruction::LRem => bytes.write_u8(LREM)?,
                Instruction::FRem => bytes.write_u8(FREM)?,
                Instruction::DRem => bytes.write_u8(DREM)?,
                Instruction::INeg => bytes.write_u8(INEG)?,
                Instruction::LNeg => bytes.write_u8(LNEG)?,
                Instruction::FNeg => bytes.write_u8(FNEG)?,
                Instruction::DNeg => bytes.write_u8(DNEG)?,
                Instruction::IAnd => bytes.write_u8(IAND)?,
                Instruction::LAnd => bytes.write_u8(LAND)?,
                Instruction::IOr => bytes.write_u8(IOR)?,
                Instruction::LOr => bytes.write_u8(LOR)?,
                Instruction::LCmp => bytes.write_u8(LCMP)?,
                Instruction::IfEq(offset) => {
                    bytes.write_u8(IFEQ)?;
                    bytes.write_i16::<BigEndian>(*offset)?;
                }
                Instruction::IfNe(offset) => {
                    bytes.write_u8(IFNE)?;
                    bytes.write_i16::<BigEndian>(*offset)?;
                }
                Instruction::IfLt(offset) => {
                    bytes.write_u8(IFLT)?;
                    bytes.write_i16::<BigEndian>(*offset)?;
                }
                Instruction::IfGe(offset) => {
                    bytes.write_u8(IFGE)?;
                    bytes.write_i16::<BigEndian>(*offset)?;
                }
                Instruction::IfGt(offset) => {
                    bytes.write_u8(IFGT)?;
                    bytes.write_i16::<BigEndian>(*offset)?;
                }
                Instruction::IfLe(offset) => {
                    bytes.write_u8(IFLE)?;
                    bytes.write_i16::<BigEndian>(*offset)?;
                }
                Instruction::IfICmpEq(offset) => {
                    bytes.write_u8(IF_ICMPEQ)?;
                    bytes.write_i16::<BigEndian>(*offset)?;
                }
                Instruction::IfICmpNe(offset) => {
                    bytes.write_u8(IF_ICMPNE)?;
                    bytes.write_i16::<BigEndian>(*offset)?;
                }
                Instruction::IfICmpLt(offset) => {
                    bytes.write_u8(IF_ICMPLT)?;
                    bytes.write_i16::<BigEndian>(*offset)?;
                }
                Instruction::IfICmpGe(offset) => {
                    bytes.write_u8(IF_ICMPGE)?;
                    bytes.write_i16::<BigEndian>(*offset)?;
                }
                Instruction::IfICmpGt(offset) => {
                    bytes.write_u8(IF_ICMPGT)?;
                    bytes.write_i16::<BigEndian>(*offset)?;
                }
                Instruction::IfICmpLe(offset) => {
                    bytes.write_u8(IF_ICMPLE)?;
                    bytes.write_i16::<BigEndian>(*offset)?;
                }
                Instruction::Goto(offset) => {
                    bytes.write_u8(GOTO)?;
                    bytes.write_i16::<BigEndian>(*offset)?;
                }
                Instruction::IReturn => bytes.write_u8(IRETURN)?,
                Instruction::LReturn => bytes.write_u8(LRETURN)?,
                Instruction::FReturn => bytes.write_u8(FRETURN)?,
                Instruction::DReturn => bytes.write_u8(DRETURN)?,
                Instruction::AReturn => bytes.write_u8(ARETURN)?,
                Instruction::Return => bytes.write_u8(RETURN)?,
                Instruction::InvokeVirtual(class, method, desc) => {
                    let index = self.constant_pool.add_methodref(class, method, desc);
                    bytes.write_u8(INVOKEVIRTUAL)?;
                    bytes.write_u16::<BigEndian>(index)?;
                }
                Instruction::InvokeSpecial(class, method, desc) => {
                    let index = self.constant_pool.add_methodref(class, method, desc);
                    bytes.write_u8(INVOKESPECIAL)?;
                    bytes.write_u16::<BigEndian>(index)?;
                }
                Instruction::InvokeStatic(class, method, desc) => {
                    let index = self.constant_pool.add_methodref(class, method, desc);
                    bytes.write_u8(INVOKESTATIC)?;
                    bytes.write_u16::<BigEndian>(index)?;
                }
                Instruction::GetStatic(class, field, desc) => {
                    let index = self.constant_pool.add_fieldref(class, field, desc);
                    bytes.write_u8(GETSTATIC)?;
                    bytes.write_u16::<BigEndian>(index)?;
                }
                Instruction::PutStatic(class, field, desc) => {
                    let index = self.constant_pool.add_fieldref(class, field, desc);
                    bytes.write_u8(PUTSTATIC)?;
                    bytes.write_u16::<BigEndian>(index)?;
                }
                Instruction::GetField(class, field, desc) => {
                    let index = self.constant_pool.add_fieldref(class, field, desc);
                    bytes.write_u8(GETFIELD)?;
                    bytes.write_u16::<BigEndian>(index)?;
                }
                Instruction::PutField(class, field, desc) => {
                    let index = self.constant_pool.add_fieldref(class, field, desc);
                    bytes.write_u8(PUTFIELD)?;
                    bytes.write_u16::<BigEndian>(index)?;
                }
                Instruction::New(class) => {
                    let index = self.constant_pool.add_class(class);
                    bytes.write_u8(NEW)?;
                    bytes.write_u16::<BigEndian>(index)?;
                }
            }
        }
        
        Ok(())
    }
}

pub struct MethodBuilder<'a> {
    class: &'a mut ClassWriter,
    name: String,
    descriptor: String,
    access_flags: u16,
    code: Option<Vec<Instruction>>,
    max_stack: u16,
    max_locals: u16,
}

impl<'a> MethodBuilder<'a> {
    pub fn set_descriptor(&mut self, descriptor: impl Into<String>) {
        self.descriptor = descriptor.into();
    }
    
    pub fn set_access_flags(&mut self, flags: u16) {
        self.access_flags = flags;
    }
    
    pub fn set_code(&mut self, code: Vec<Instruction>) {
        self.code = Some(code);
    }
    
    pub fn set_max_stack(&mut self, max: u16) {
        self.max_stack = max;
    }
    
    pub fn set_max_locals(&mut self, max: u16) {
        self.max_locals = max;
    }
}

impl<'a> Drop for MethodBuilder<'a> {
    fn drop(&mut self) {
        self.class.methods.push(MethodInfo {
            access_flags: self.access_flags,
            name: self.name.clone(),
            descriptor: self.descriptor.clone(),
            code: self.code.take(),
            max_stack: self.max_stack,
            max_locals: self.max_locals,
        });
    }
}
