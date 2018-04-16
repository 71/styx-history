namespace Styx.Library

open Styx

module Types =
    let inline private simpleType ns n dotnet =
        { Namespace = ns ; Name = n ; Properties = [] ; ILType = dotnet }

    let Void = TypeDefinition.Void
    let Never = TypeDefinition.Never
    let String = simpleType "System" "String" typeof<string>
    let Boolean = simpleType "System" "Boolean" typeof<bool>
    let Char = simpleType "System" "Char" typeof<char>
    let Int = simpleType "System" "Int" typeof<int64>
    let UInt = simpleType "System" "UInt" typeof<uint64>
    let Double = simpleType "System" "Double" typeof<double>

    let TypeDefinition = simpleType "Styx" "TypeDefinition" typeof<TypeDefinition>
    let TypeInstance = simpleType "Styx" "TypeInstance" typeof<Type>
    let Type = TypeInstance

    let All = [| Void ; Never ; String ; Boolean ; Char ; Int ; UInt ; Double ; TypeDefinition ; TypeInstance |]
