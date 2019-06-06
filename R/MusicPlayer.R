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
#   - this file contains a simple music player implemented using the      #
#     `audio` package                                                     #
#                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Music player class ------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

MusicPlayer <- R6::R6Class("MusicPlayer",
    private = list(
        ..current = list(),

        ..status = NULL,
        
        ..wave = NULL,
        
        ..player = NULL,
        
        ..songs = list(
            "Redbone - Come And Get Your Love" = "assets/redbone-come-and-get-your-love.wav"
        ),

        
        ..determine.song = function(song.name) {
            # Get the song path.
            path <- private$..songs[[song.name]]
            name <- song.name

            # In case a non-existing song was requested.
            if(is.null(path)) {
                path <- private$..songs[[1]]
                name <- names(which(private$..songs == path))
            }
            
            # The song details.
            song <- list(
                path = path,
                name = name
            )

            return(song)
        }
    ),


    public = list(
        initialize = function(load = TRUE, ...) {
            if(load) {
                # Load the song wave.
                self$load(...)
            }
        },


        load = function(song.name = "Redbone - Come And Get Your Love") {
            # Decide about what to load.
            private$..current <- private$..determine.song(song.name)

            # Load the wave.
            private$..wave <- audio::load.wave(private$..current$path)

            # Mark the status as loaded.
            private$..status = "ready"

            # Let the user know what song was loaded.
            cat("Loaded ", crayon::blue(private$..current$name), ".\n", sep = "")
        },


        play = function() {
            if(!is.null(private$..status) && private$..status == "ready") {
                private$..player <- audio::play(private$..wave)

                # Update the status.
                private$..status <- "playing"

                # Let the user know what song is playing.
                cat("Playing ", crayon::blue(private$..current$name), ".\n", sep = "")
            }
        },


        pause = function() {
            if(!is.null(private$..status) && private$..status == "playing") {
                # Pause the player.
                audio::pause(private$..player)

                # Update the status.
                private$..status <- "paused"

                # Let the user know what song is paused.
                cat("Paused ", crayon::blue(private$..current$name), ".\n", sep = "")  
            }
        },


        resume = function() {
            if(!is.null(private$..status) && private$..status == "paused") {
                # Resume the player.
                audio::resume(private$..player)

                # Update the status.
                private$..status = "playing"
                
                # Some user feedback.
                cat("Resumed ", crayon::blue(private$..current$name), ".\n", sep = "")
            } 
        },


        stop = function() {
            # Resume the audio instance.
            audio::close.audioInstance(private$..player)
            
            # Update the private members.
            private$..status = NULL
            private$..current = list()
            private$..wave = NULL
            private$..player = NULL

            # Some user feedback.
            cat("Stopped and reset the audio player.\n")
        },


        current = function() {
            cat("Currently playing ", crayon::blue(private$..current$name), ".\n", sep = "")
        }
    ),


    active = list(
        status = function() {
            return(private$..status)
        }
    ) 
)



# End of file.
