## ===-- llvm-c/Support.h - C Interface Types declarations ---------*- C -*-===*\
## |*                                                                            *|
## |*                     The LLVM Compiler Infrastructure                       *|
## |*                                                                            *|
## |* This file is distributed under the University of Illinois Open Source      *|
## |* License. See LICENSE.TXT for details.                                      *|
## |*                                                                            *|
## |*===----------------------------------------------------------------------===*|
## |*                                                                            *|
## |* This file defines types used by the the C interface to LLVM.               *|
## |*                                                                            *|
## \*===----------------------------------------------------------------------===

## *
##  @defgroup LLVMCSupportTypes Types and Enumerations
## 
##  @{
## 

type
  Bool* = cint

##  Opaque types.
## *
##  LLVM uses a polymorphic type hierarchy which C cannot represent, therefore
##  parameters must be passed as base types. Despite the declared types, most
##  of the functions provided operate only on branches of the type hierarchy.
##  The declared parameter names are descriptive and specify which type is
##  required. Additionally, each type hierarchy is documented along with the
##  functions that operate upon it. For more detail, refer to LLVM's C++ code.
##  If in doubt, refer to Core.cpp, which performs parameter downcasts in the
##  form unwrap<RequiredType>(Param).
## 
## *
##  Used to pass regions of memory through LLVM interfaces.
## 
##  @see llvm::MemoryBuffer
## 

type
  MemoryBufferRef* = ptr OpaqueMemoryBuffer

## *
##  The top-level container for all LLVM global data. See the LLVMContext class.
## 

type
  ContextRef* = ptr OpaqueContext

## *
##  The top-level container for all other LLVM Intermediate Representation (IR)
##  objects.
## 
##  @see llvm::Module
## 

type
  ModuleRef* = ptr OpaqueModule

## *
##  Each value in the LLVM IR has a type, an LLVMTypeRef.
## 
##  @see llvm::Type
## 

type
  TypeRef* = ptr OpaqueType

## *
##  Represents an individual value in LLVM IR.
## 
##  This models llvm::Value.
## 

type
  ValueRef* = ptr OpaqueValue

## *
##  Represents a basic block of instructions in LLVM IR.
## 
##  This models llvm::BasicBlock.
## 

type
  BasicBlockRef* = ptr OpaqueBasicBlock

## *
##  Represents an LLVM Metadata.
## 
##  This models llvm::Metadata.
## 

type
  MetadataRef* = ptr OpaqueMetadata

## *
##  Represents an LLVM basic block builder.
## 
##  This models llvm::IRBuilder.
## 

type
  BuilderRef* = ptr OpaqueBuilder

## *
##  Represents an LLVM debug info builder.
## 
##  This models llvm::DIBuilder.
## 

type
  DIBuilderRef* = ptr OpaqueDIBuilder

## *
##  Interface used to provide a module to JIT or interpreter.
##  This is now just a synonym for llvm::Module, but we have to keep using the
##  different type to keep binary compatibility.
## 

type
  ModuleProviderRef* = ptr OpaqueModuleProvider

## * @see llvm::PassManagerBase

type
  PassManagerRef* = ptr OpaquePassManager

## * @see llvm::PassRegistry

type
  PassRegistryRef* = ptr OpaquePassRegistry

## *
##  Used to get the users and usees of a Value.
## 
##  @see llvm::Use

type
  UseRef* = ptr OpaqueUse

## *
##  Used to represent an attributes.
## 
##  @see llvm::Attribute
## 

type
  AttributeRef* = ptr OpaqueAttributeRef

## *
##  @see llvm::DiagnosticInfo
## 

type
  DiagnosticInfoRef* = ptr OpaqueDiagnosticInfo

## *
##  @}
## 
