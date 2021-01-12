.checkMainArgs <- function(mainArgs) {
    inArgs <- names(mainArgs)[-1] # 1st member name of calling function
    validArgs <- .getValidArgs()
    invalid <- setdiff(inArgs,validArgs)
    if (length(invalid) > 0) {
        for (i in seq_len(length(invalid)))
            warnwrap("Unknown input argument to sitadela: ",invalid[i],
                " ...Ignoring...",now=TRUE)
    }
}

.checkTextArgs <- function(argName,argValue,argList,multiarg=FALSE) {
    if (multiarg) {
        argValue <- tolower(argValue)
        if (!all(argValue %in% argList))
            stopwrap("\"",argName,"\""," parameter must be one or more of ",
                paste(paste("\"",argList,sep=""),collapse="\", "),"\"!")
    }
    else {
        argSave <- argValue[1]
        argValue <- tolower(argValue[1])    
        if (!(argValue %in% argList))
            stopwrap("\"",argName,"\""," parameter must be one of ",
                paste(paste("\"",argList,sep=""),collapse="\", "),"\"!")
    }
}

.checkNumArgs <- function(argName,argValue,argType,argBounds,direction) {
    switch(argType,
        numeric = {
            if (!is.numeric(argValue))
                stopwrap("\"",argName,"\"",
                    " parameter must be a numeric value!")
            if (!missing(argBounds)) {
                switch(direction,
                    both = {
                        if (argValue<argBounds[1] ||
                            argValue>argBounds[2])
                            stopwrap("\"",argName,"\""," parameter must be a ",
                                "numeric ","value larger than or equal to ",
                                argBounds[1]," and smaller than or equal to ",
                                argBounds[2],"!")
                    },
                    botheq = {
                        if (argValue<=argBounds[1] || argValue>=argBounds[2])
                            stopwrap("\"",argName,"\""," parameter must be a ",
                                "numeric value larger than ",argBounds[1],
                                " and smaller than ",argBounds[2],"!")
                    },
                    gt = {
                        if (argValue<=argBounds[1])
                            stopwrap("\"",argName,"\""," parameter must be a ",
                                "numeric value greater than ",argBounds[1],"!")
                    },
                    lt = {
                        if (argValue>=argBounds[1])
                            stopwrap("\"",argName,"\""," parameter must be a ",
                                "numeric value lower than ",argBounds[1],"!")
                    },
                    gte = {
                        if (argValue<argBounds[1])
                            stopwrap("\"",argName,"\""," parameter must be a ",
                                "numeric value greater than or equal to ",
                                argBounds[1],"!")
                    },
                    lte = {
                        if (argValue>argBounds[1])
                            stopwrap("\"",argName,"\""," parameter must be a ",
                                "numeric value lower than or equal to ",
                                argBounds[1],"!")
                    }
                )
            }
        },
        integer = {
            if (!is.integer(argValue))
                stopwrap("\"",argName,"\""," parameter must be an integer!")
            if (!missing(argBounds)) {
                switch(direction,
                    both = {
                        if (argValue<argBounds[1] ||
                            argValue>argBounds[2])
                            stopwrap("\"",argName,"\""," parameter must be ",
                                "an integer larger than or equal to ",
                                argBounds[1]," and smaller than or equal to ",
                                argBounds[2],"!")
                    },
                    botheq = {
                        if (argValue<=argBounds[1] || argValue>=argBounds[2])
                            stopwrap("\"",argName,"\""," parameter must be ",
                                "an integer larger than ",argBounds[1],
                                " and smaller than ",argBounds[2],"!")
                    },
                    gt = {
                        if (argValue<=argBounds[1])
                            stopwrap("\"",argName,"\""," parameter must be ",
                                "an integer greater than ",argBounds[1],"!")
                    },
                    lt = {
                        if (argValue>=argBounds[1])
                            stopwrap("\"",argName,"\""," parameter must be ",
                                "an integer lower than ",argBounds[1],"!")
                    },
                    gte = {
                        if (argValue<argBounds[1])
                            stopwrap("\"",argName,"\""," parameter must be ",
                                "an integer greater than or equal to ",
                                argBounds[1],"!")
                    },
                    lte = {
                        if (argValue>argBounds[1])
                            stopwrap("\"",argName,"\""," parameter must be ",
                                "an integer lower than or equal to ",
                                argBounds[1],"!")
                    }
                )
            }
        }
    )
}

.getValidArgs <- function() {
    return(c("organisms","sources","db","forceDownload","rc"))
}
