use byteorder::{BigEndian, WriteBytesExt};
use std::collections::HashMap;
use std::io::Write;

// Constant pool tags
const CONSTANT_UTF8: u8 = 1;
const CONSTANT_INTEGER: u8 = 3;
const CONSTANT_FLOAT: u8 = 4;
const CONSTANT_LONG: u8 = 5;
const CONSTANT_DOUBLE: u8 = 6;
const CONSTANT_CLASS: u8 = 7;
const CONSTANT_STRING: u8 = 8;
const CONSTANT_FIELDREF: u8 = 9;
const CONSTANT_METHODREF: u8 = 10;
const CONSTANT_NAME_AND_TYPE: u8 = 12;

#[derive(Debug, Clone)]
pub enum ConstantPoolEntry {
    Utf8(String),
    Integer(i32),
    Float(f32),
    Long(i64),
    Double(f64),
    Class(u16),             // name_index
    String(u16),            // utf8_index
    Fieldref(u16, u16),     // class_index, name_and_type_index
    Methodref(u16, u16),    // class_index, name_and_type_index
    NameAndType(u16, u16),  // name_index, descriptor_index
    Placeholder,            // Takes a slot but is not written (for Long/Double second slot)
}

pub struct ConstantPool {
    entries: Vec<ConstantPoolEntry>,
    utf8_map: HashMap<String, u16>,
    class_map: HashMap<String, u16>,
    string_map: HashMap<String, u16>,
    nat_map: HashMap<(String, String), u16>,
    methodref_map: HashMap<(String, String, String), u16>,
    fieldref_map: HashMap<(String, String, String), u16>,
    integer_map: HashMap<i32, u16>,
    long_map: HashMap<i64, u16>,
    float_map: HashMap<u32, u16>, // Store as bits for proper equality
    double_map: HashMap<u64, u16>, // Store as bits for proper equality
}

impl ConstantPool {
    pub fn new() -> Self {
        Self {
            entries: Vec::new(),
            utf8_map: HashMap::new(),
            class_map: HashMap::new(),
            string_map: HashMap::new(),
            nat_map: HashMap::new(),
            methodref_map: HashMap::new(),
            fieldref_map: HashMap::new(),
            integer_map: HashMap::new(),
            long_map: HashMap::new(),
            float_map: HashMap::new(),
            double_map: HashMap::new(),
        }
    }
    
    pub fn add_utf8(&mut self, value: impl Into<String>) -> u16 {
        let value = value.into();
        if let Some(&index) = self.utf8_map.get(&value) {
            return index;
        }
        
        self.entries.push(ConstantPoolEntry::Utf8(value.clone()));
        let index = self.entries.len() as u16;
        self.utf8_map.insert(value, index);
        index
    }
    
    pub fn add_class(&mut self, name: impl Into<String>) -> u16 {
        let name = name.into();
        if let Some(&index) = self.class_map.get(&name) {
            return index;
        }
        
        let name_index = self.add_utf8(name.clone());
        self.entries.push(ConstantPoolEntry::Class(name_index));
        let index = self.entries.len() as u16;
        self.class_map.insert(name, index);
        index
    }
    
    pub fn add_string(&mut self, value: impl Into<String>) -> u16 {
        let value = value.into();
        if let Some(&index) = self.string_map.get(&value) {
            return index;
        }
        
        let utf8_index = self.add_utf8(value.clone());
        self.entries.push(ConstantPoolEntry::String(utf8_index));
        let index = self.entries.len() as u16;
        self.string_map.insert(value, index);
        index
    }
    
    pub fn add_name_and_type(&mut self, name: impl Into<String>, descriptor: impl Into<String>) -> u16 {
        let name = name.into();
        let descriptor = descriptor.into();
        let key = (name.clone(), descriptor.clone());
        
        if let Some(&index) = self.nat_map.get(&key) {
            return index;
        }
        
        let name_index = self.add_utf8(name);
        let desc_index = self.add_utf8(descriptor);
        self.entries.push(ConstantPoolEntry::NameAndType(name_index, desc_index));
        let index = self.entries.len() as u16;
        self.nat_map.insert(key, index);
        index
    }
    
    pub fn add_methodref(&mut self, class: impl Into<String>, name: impl Into<String>, descriptor: impl Into<String>) -> u16 {
        let class = class.into();
        let name = name.into();
        let descriptor = descriptor.into();
        let key = (class.clone(), name.clone(), descriptor.clone());
        
        if let Some(&index) = self.methodref_map.get(&key) {
            return index;
        }
        
        let class_index = self.add_class(class);
        let nat_index = self.add_name_and_type(name, descriptor);
        self.entries.push(ConstantPoolEntry::Methodref(class_index, nat_index));
        let index = self.entries.len() as u16;
        self.methodref_map.insert(key, index);
        index
    }
    
    pub fn add_fieldref(&mut self, class: impl Into<String>, name: impl Into<String>, descriptor: impl Into<String>) -> u16 {
        let class = class.into();
        let name = name.into();
        let descriptor = descriptor.into();
        let key = (class.clone(), name.clone(), descriptor.clone());
        
        if let Some(&index) = self.fieldref_map.get(&key) {
            return index;
        }
        
        let class_index = self.add_class(class);
        let nat_index = self.add_name_and_type(name, descriptor);
        self.entries.push(ConstantPoolEntry::Fieldref(class_index, nat_index));
        let index = self.entries.len() as u16;
        self.fieldref_map.insert(key, index);
        index
    }
    
    pub fn add_integer(&mut self, value: i32) -> u16 {
        if let Some(&index) = self.integer_map.get(&value) {
            return index;
        }
        
        self.entries.push(ConstantPoolEntry::Integer(value));
        let index = self.entries.len() as u16;
        self.integer_map.insert(value, index);
        index
    }
    
    pub fn add_long(&mut self, value: i64) -> u16 {
        if let Some(&index) = self.long_map.get(&value) {
            return index;
        }
        
        self.entries.push(ConstantPoolEntry::Long(value));
        let index = self.entries.len() as u16;
        self.long_map.insert(value, index);
        // Long and Double take two slots
        self.entries.push(ConstantPoolEntry::Placeholder);
        index
    }
    
    pub fn add_float(&mut self, value: f32) -> u16 {
        let bits = value.to_bits();
        if let Some(&index) = self.float_map.get(&bits) {
            return index;
        }
        
        self.entries.push(ConstantPoolEntry::Float(value));
        let index = self.entries.len() as u16;
        self.float_map.insert(bits, index);
        index
    }
    
    pub fn add_double(&mut self, value: f64) -> u16 {
        let bits = value.to_bits();
        if let Some(&index) = self.double_map.get(&bits) {
            return index;
        }
        
        self.entries.push(ConstantPoolEntry::Double(value));
        let index = self.entries.len() as u16;
        self.double_map.insert(bits, index);
        // Long and Double take two slots
        self.entries.push(ConstantPoolEntry::Placeholder);
        index
    }
    
    // Getter methods - return existing indices without adding new entries
    pub fn get_utf8_index(&self, value: &str) -> u16 {
        *self.utf8_map.get(value).expect("UTF8 entry not found in constant pool")
    }
    
    pub fn get_class_index(&self, name: &str) -> u16 {
        *self.class_map.get(name).expect("Class entry not found in constant pool")
    }
    
    pub fn get_string_index(&self, value: &str) -> u16 {
        *self.string_map.get(value).expect("String entry not found in constant pool")
    }
    
    pub fn write<W: Write>(&self, writer: &mut W) -> std::io::Result<()> {
        // Write count (number of entries + 1)
        writer.write_u16::<BigEndian>((self.entries.len() + 1) as u16)?;
        
        // Write entries
        for entry in &self.entries {
            match entry {
                ConstantPoolEntry::Utf8(s) => {
                    writer.write_u8(CONSTANT_UTF8)?;
                    writer.write_u16::<BigEndian>(s.len() as u16)?;
                    writer.write_all(s.as_bytes())?;
                }
                ConstantPoolEntry::Integer(v) => {
                    writer.write_u8(CONSTANT_INTEGER)?;
                    writer.write_i32::<BigEndian>(*v)?;
                }
                ConstantPoolEntry::Float(v) => {
                    writer.write_u8(CONSTANT_FLOAT)?;
                    writer.write_f32::<BigEndian>(*v)?;
                }
                ConstantPoolEntry::Long(v) => {
                    writer.write_u8(CONSTANT_LONG)?;
                    writer.write_i64::<BigEndian>(*v)?;
                }
                ConstantPoolEntry::Double(v) => {
                    writer.write_u8(CONSTANT_DOUBLE)?;
                    writer.write_f64::<BigEndian>(*v)?;
                }
                ConstantPoolEntry::Class(name_index) => {
                    writer.write_u8(CONSTANT_CLASS)?;
                    writer.write_u16::<BigEndian>(*name_index)?;
                }
                ConstantPoolEntry::String(utf8_index) => {
                    writer.write_u8(CONSTANT_STRING)?;
                    writer.write_u16::<BigEndian>(*utf8_index)?;
                }
                ConstantPoolEntry::Fieldref(class_index, nat_index) => {
                    writer.write_u8(CONSTANT_FIELDREF)?;
                    writer.write_u16::<BigEndian>(*class_index)?;
                    writer.write_u16::<BigEndian>(*nat_index)?;
                }
                ConstantPoolEntry::Methodref(class_index, nat_index) => {
                    writer.write_u8(CONSTANT_METHODREF)?;
                    writer.write_u16::<BigEndian>(*class_index)?;
                    writer.write_u16::<BigEndian>(*nat_index)?;
                }
                ConstantPoolEntry::NameAndType(name_index, desc_index) => {
                    writer.write_u8(CONSTANT_NAME_AND_TYPE)?;
                    writer.write_u16::<BigEndian>(*name_index)?;
                    writer.write_u16::<BigEndian>(*desc_index)?;
                }
                ConstantPoolEntry::Placeholder => {
                    // Don't write anything for placeholder entries
                    // They exist only to take up a slot in the numbering
                }
            }
        }
        
        Ok(())
    }
}

impl Default for ConstantPool {
    fn default() -> Self {
        Self::new()
    }
}
