namespace FSCC.Service

open IntelliFactory.WebSharper

module Remoting =

    [<Remote>]
    let Process input =
        async {
            return "You said: " + input
        }
