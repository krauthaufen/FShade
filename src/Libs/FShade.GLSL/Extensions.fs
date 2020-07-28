namespace FShade

open System
open Aardvark.Base
open FShade.Imperative
open FShade.GLSL

[<AutoOpen>]
module Backends =
    let glsl410 =
        Backend.Create {
            version                 = GLSLVersion(4,1,0)
            enabledExtensions       = Set.ofList [ ]
            createUniformBuffers    = true
            bindingMode             = BindingMode.PerKind
            createDescriptorSets    = false
            stepDescriptorSets      = false
            createInputLocations    = true
            createOutputLocations   = true
            createPassingLocations  = true
            createPerStageUniforms  = false
            reverseMatrixLogic      = true
            depthWriteMode          = false
            useInOut                = true
        }

    let glsl430 =
        Backend.Create {
            version                 = GLSLVersion(4,3,0)
            enabledExtensions       = Set.ofList [ ]
            createUniformBuffers    = true
            bindingMode             = BindingMode.PerKind
            createDescriptorSets    = false
            stepDescriptorSets      = false
            createInputLocations    = true
            createOutputLocations   = true
            createPassingLocations  = true
            createPerStageUniforms  = false
            reverseMatrixLogic      = true
            depthWriteMode          = true
            useInOut                = true
        }

    let glsl120 =
        Backend.Create {
            version                 = GLSLVersion(1,2,0)
            enabledExtensions       = Set.empty
            createUniformBuffers    = false
            bindingMode             = BindingMode.None
            createDescriptorSets    = false
            stepDescriptorSets      = false
            createInputLocations    = false
            createOutputLocations   = false
            createPassingLocations  = false
            createPerStageUniforms  = false
            reverseMatrixLogic      = true
            depthWriteMode          = false
            useInOut                = false
        }

    let glslVulkan =
        Backend.Create {
            version                 = GLSLVersion(4,5,0)
            enabledExtensions       = Set.ofList [ "GL_ARB_tessellation_shader"; "GL_ARB_separate_shader_objects"; "GL_ARB_shading_language_420pack" ]
            createUniformBuffers    = true
            bindingMode             = BindingMode.Global
            createDescriptorSets    = true
            stepDescriptorSets      = false
            createInputLocations    = true
            createOutputLocations   = true
            createPassingLocations  = true
            createPerStageUniforms  = true
            reverseMatrixLogic      = true
            depthWriteMode          = true
            useInOut                = true
        }

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ModuleCompiler =

        let private containsCompute (m : Module) =
            m.entries |> List.exists (fun e -> e.decorations |> List.exists (function EntryDecoration.Stages { self = ShaderStage.Compute } -> true | _ -> false))

        let compileGLSL (cfg : Backend) (module_ : Module) =
            let cfg =
                if containsCompute module_ then
                    Backend.Create cfg.Config
                else
                    cfg

            module_ 
                |> ModuleCompiler.compile cfg 
                |> Assembler.assemble cfg
                
        let compileGLSL120 (module_ : Module) =
            compileGLSL glsl120 module_

        let compileGLSL410 (module_ : Module) =
            compileGLSL glsl410 module_
            
        let compileGLSL430 (module_ : Module) =
            compileGLSL glsl430 module_

        let compileGLSLVulkan (module_ : Module) =
            compileGLSL glslVulkan module_