# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                              _                                          #
#                             | |                                         #
#                 _ __    ___ | |_  _ __    __ _ __      __               #
#                | '_ \  / _ \| __|| '_ \  / _` |\ \ /\ / /               #
#                | | | ||  __/| |_ | |_) || (_| | \ V  V /                #
#                |_| |_| \___| \__|| .__/  \__,_|  \_/\_/                 #
#                                  | |                                    #
#                                  |_|                                    #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                                                                         #
# File contributors:                                                      #
#   - M.A. Constantin                                                     #
#                                                                         #
# File description:                                                       #
#   - contains a class that defines a converter between aliases and       #
#     types and vice-versa                                                #
#                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# TypeHandler class -------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

TypeHandler <- R6::R6Class("TypeHandler",

    private = list(
        ..type = NULL,
        ..blueprint = NULL,
        ..ancestor = NULL,
        ..alias = NULL,


        # Find ancestor of an `R6ClassGenerator`.
        ..find.ancestor = function(type) {
            # Evaluate the type.
            blueprint <- eval(as.symbol(type))

            # Get the parent class.
            inherit <- blueprint$inherit

            # If more parents exist, recall, else handle type setting.
            if(!is.null(inherit)) { 
                Recall(inherit) 
            
            # Only set the ancestor if at least one parent class exists.
            } else {
                # Convert to character.
                type <- as.character(type)
                
                # Check that the current type under evaluation is not the same as from instantiation.
                if(type != private$..type) {
                    private$..ancestor <- type
                }
            }
        },


        # Find alias based on provided type.
        ..find.alias = function() {
            if(!is.null(private$..ancestor)) {
                # Convert to blueprint.
                blueprint <- eval(as.symbol(private$..ancestor))

                # Is it an ancestor with aliases?
                if(!is.null(blueprint$..ALIASES..)) {
                    # Then check every alias for a match.
                    for(alias in blueprint$..ALIASES..) {
                       if(alias$class$classname == private$..type) {
                           private$..alias <- alias$name
                       }
                    }
                }
            }
        },


        # Set the blueprint based on the type.
        ..set.blueprint = function() {
            # Evaluate the type.
            blueprint <- eval(as.symbol(private$..type))

            # Type check.
            assert(class(blueprint) == "R6ClassGenerator", ..ERRORS..$incorrect.object.type)

            # Set the class field.
            private$..blueprint <- blueprint
        },


        # Solve the problem using a type.
        ..from.type = function(input) {
            # Set the type.
            private$..type <- input

            # Set the blueprint.
            private$..set.blueprint()

            # Find and set the ancestor.
            private$..find.ancestor(input)

            # Find and set the alias.
            private$..find.alias()
        },


        # Solve the problem using an alias.
        ..from.alias = function(input, ancestor) {
            # Parse the ancestor as a class generator.
            ancestor.blueprint <- eval(as.symbol(ancestor))

            # Type check the ancestor.
            assert(class(ancestor.blueprint) == "R6ClassGenerator", ..ERRORS..$incorrect.object.type)

            # Set the alias.
            private$..alias <- input

            # Set the ancestor.
            private$..ancestor <- ancestor

            # Set the type.
            private$..type <- ancestor.blueprint$..ALIASES..[[private$..alias]]$class$classname

            # Set the type blueprint.
            private$..blueprint <- ancestor.blueprint$..ALIASES..[[private$..alias]]$class
        }
    ),


    public = list(
        # Constructor.
        initialize = function(input, ..., is.alias = FALSE) {
            # Decide from what perspective to tackle the problem.
            if(is.alias) {
                private$..from.alias(input, ...)
            } else {
                private$..from.type(input)
            }
        }
    ),


    active = list(
        type = function() {
            return(private$..type)
        },


        blueprint = function() {
            return(private$..blueprint)
        },


        ancestor = function() {
            return(private$..ancestor)
        },


        alias = function() {
            return(private$..alias)
        }
    )
)



# End of file.
