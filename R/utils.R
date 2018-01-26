#############################################################################
##
##  Utility functions
##
#############################################################################

## --------------------------------------------------------------------------
## Copies from non-exported functions in utils-3.4.1
##

my.getHelpFile <- function (file) 
{
    path <- dirname(file)
    dirpath <- dirname(path)
    if (!file.exists(dirpath)) 
        stop(gettextf("invalid %s argument", sQuote("file")), 
             domain = NA)
    pkgname <- basename(dirpath)
    RdDB <- file.path(path, pkgname)
    if (!file.exists(paste0(RdDB, ".rdx"))) 
        stop(gettextf("package %s exists but was not installed under R >= 2.10.0 so help cannot be accessed", 
                      sQuote(pkgname)), domain = NA)
    my.fetchRdDB(RdDB, basename(file))
}

my.fetchRdDB <- function (filebase, key = NULL) 
{
    fun <- function(db) {
        vals <- db$vals
        vars <- db$vars
        datafile <- db$datafile
        compressed <- db$compressed
        envhook <- db$envhook
        fetch <- function(key) lazyLoadDBfetch(vals[key][[1L]], 
                                               datafile, compressed, envhook)
        if (length(key)) {
            if (!key %in% vars) 
                stop(gettextf("No help on %s found in RdDB %s", 
                              sQuote(key), sQuote(filebase)), domain = NA)
            fetch(key)
        }
        else {
            res <- lapply(vars, fetch)
            names(res) <- vars
            res
        }
    }
    res <- lazyLoadDBexec(filebase, fun)
    if (length(key)) 
        res
    else invisible(res)
}

# ---------------------------------------------------------------------------

