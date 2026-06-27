namespace FShade

open System
open Aardvark.Base
open FShade.Imperative
open FShade.GLSL

[<AutoOpen>]
module Backends =
    let glsl410 =
        Backend.Create {
            version                     = GLSLVersion(4,1,0)
            enabledExtensions           = Set.ofList [ ]
            availableExtensions         = Map.empty
            createUniformBuffers        = true
            pushConstants               = false
            bindingMode                 = BindingMode.PerKind
            createDescriptorSets        = false
            stepDescriptorSets          = false
            createInputLocations        = true
            createOutputLocations       = true
            createPassingLocations      = true
            createPerStageUniforms      = false
            reverseMatrixLogic          = true
            reverseTessellationWinding  = false
            depthWriteMode              = false
            useInOut                    = true
            separateTexturesAndSamplers = false
        }

    let glsl430 =
        Backend.Create {
            version                     = GLSLVersion(4,3,0)
            enabledExtensions           = Set.ofList [ ]
            availableExtensions         = Map.empty
            createUniformBuffers        = true
            pushConstants               = false
            bindingMode                 = BindingMode.PerKind
            createDescriptorSets        = false
            stepDescriptorSets          = false
            createInputLocations        = true
            createOutputLocations       = true
            createPassingLocations      = true
            createPerStageUniforms      = false
            reverseMatrixLogic          = true
            reverseTessellationWinding  = false
            depthWriteMode              = true
            useInOut                    = true
            separateTexturesAndSamplers = false
        }

    let glsl120 =
        Backend.Create {
            version                     = GLSLVersion(1,2,0)
            enabledExtensions           = Set.empty
            availableExtensions         = Map.empty
            createUniformBuffers        = false
            pushConstants               = false
            bindingMode                 = BindingMode.None
            createDescriptorSets        = false
            stepDescriptorSets          = false
            createInputLocations        = false
            createOutputLocations       = false
            createPassingLocations      = false
            createPerStageUniforms      = false
            reverseMatrixLogic          = true
            reverseTessellationWinding  = false
            depthWriteMode              = false
            useInOut                    = false
            separateTexturesAndSamplers = false
        }

    let glslVulkan =
        let enabledExtension =
            Set.ofList [
                GLSLExtension.ARBTessellationShader
                GLSLExtension.ARBSeparateShaderObjects
                GLSLExtension.ARBShadingLanguage420pack
            ]

        Backend.Create {
            version                     = GLSLVersion(4,6,0)
            enabledExtensions           = enabledExtension
            availableExtensions         = Map.empty
            createUniformBuffers        = true
            pushConstants               = true
            bindingMode                 = BindingMode.Global
            createDescriptorSets        = true
            stepDescriptorSets          = true
            createInputLocations        = true
            createOutputLocations       = true
            createPassingLocations      = true
            createPerStageUniforms      = false
            reverseMatrixLogic          = true
            reverseTessellationWinding  = true
            depthWriteMode              = true
            useInOut                    = true
            separateTexturesAndSamplers = false
        }

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ModuleCompiler =

        let private containsCompute (m : Module) =
            m.Entries |> List.exists (fun e -> e.decorations |> List.exists (function EntryDecoration.Stages ShaderStageDescription.Compute -> true | _ -> false))

        let compileGLSL (cfg : Backend) (module_ : Module) =
            let cfg =
                if containsCompute module_ then
                    Backend.Create cfg.Config
                else
                    cfg

            let cModule =     
                module_ 
                |> ModuleCompiler.compile cfg
                
            let cModule = 
                if cfg.Config.separateTexturesAndSamplers then SamplerSplitter.splitTexturesAndSamplers cModule
                else cModule
                
            cModule
                |> Assembler.assemble cfg
                
        let compileGLSL120 (module_ : Module) =
            compileGLSL glsl120 module_

        let compileGLSL410 (module_ : Module) =
            compileGLSL glsl410 module_
            
        let compileGLSL430 (module_ : Module) =
            compileGLSL glsl430 module_

        let compileGLSLVulkan (module_ : Module) =
            compileGLSL glslVulkan module_