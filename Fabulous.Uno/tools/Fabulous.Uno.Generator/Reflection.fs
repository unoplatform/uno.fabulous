// Copyright 2018-2019 Fabulous contributors. See LICENSE.md for license.
namespace Fabulous.Uno.Generator

open System
open System.IO
open System.Runtime.Loader
open Fabulous.CodeGen
open Fabulous.CodeGen.AssemblyReader.Models
open Fabulous.Uno.Generator.Helpers

module Reflection =    
    let loadAllAssemblies (paths: seq<string>) =
        let toFullPath p = Path.Combine(Environment.CurrentDirectory, p)
        
        paths
        |> Seq.map (toFullPath >> AssemblyLoadContext.Default.LoadFromAssemblyPath)
        |> Seq.toArray
    
    let tryGetPropertyInAssembly (assembly: System.Reflection.Assembly) (typeName, propertyName) =
        let toCleanTypeName propertyReturnType =
            propertyReturnType.ToString()
                              .Replace("[", "<")
                              .Replace("]", ">")
            |> Text.removeDotNetGenericNotation
        
        nullable {
            let! ``type`` = assembly.GetType(typeName)
            match ``type``.ContainsGenericParameters with
            | true -> return None // Generic types are not supported
            | false ->
                let dependencyPropertyFieldInfo = ``type``.GetField(propertyName + "Property")
                let dependencyPropertyInfo = ``type``.GetProperty(propertyName + "Property")
                if dependencyPropertyInfo <> null || dependencyPropertyFieldInfo <> null then
                    let filteredProperties = ``type``.GetProperties() |> Array.filter (fun m -> m.Name.Equals(propertyName))
                    return match filteredProperties with
                            | [|property|] -> Some { 
                                            Name = propertyName
                                            Type = property.GetMethod.ReturnType |> toCleanTypeName
                                            DefaultValue = null (*property.GetMetadata(``type``).DefaultValue*) }
                            | _ -> None
                else
                    return None
        }
                        
                            
    let tryGetProperty (assemblies: System.Reflection.Assembly array) (typeName, propertyName) =
        assemblies
        |> Array.tryPick (fun asm -> tryGetPropertyInAssembly asm (typeName, propertyName))