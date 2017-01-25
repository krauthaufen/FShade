namespace FShade

open System
open Aardvark.Base
open FShade.Imperative
open FShade.GLSL

[<AutoOpen>]
module Backends =
    let glsl410 =
        Backend.Create {
            version                 = Version(4,1)
            enabledExtensions       = Set.empty
            createUniformBuffers    = true
            createBindings          = false
            createDescriptorSets    = false
            createInputLocations    = true
            createPerStageUniforms  = false
            reverseMatrixLogic      = true
        }

    let glsl120 =
        Backend.Create {
            version                 = Version(1,2)
            enabledExtensions       = Set.empty
            createUniformBuffers    = false
            createBindings          = false
            createDescriptorSets    = false
            createInputLocations    = false
            createPerStageUniforms  = false
            reverseMatrixLogic      = true
        }

    let glslVulkan =
        Backend.Create {
            version                 = Version(1,4)
            enabledExtensions       = Set.ofList [ "GL_ARB_tessellation_shader"; "GL_ARB_separate_shader_objects"; "GL_ARB_shading_language_420pack" ]
            createUniformBuffers    = true
            createBindings          = true
            createDescriptorSets    = true
            createInputLocations    = true
            createPerStageUniforms  = true
            reverseMatrixLogic      = true
        }

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ModuleCompiler =
        let compileGLSL (cfg : Backend) (module_ : Module) =
            module_ 
                |> ModuleCompiler.compile cfg 
                |> Assembler.assemble cfg
                
        let compileGLSL120 (module_ : Module) =
            compileGLSL glsl120 module_

        let compileGLSL410 (module_ : Module) =
            compileGLSL glsl410 module_

        let compileGLSLVulkan (module_ : Module) =
            compileGLSL glslVulkan module_