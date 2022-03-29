# Usage in R: source("https://mac.R-project.org/bin/install.R")
# .. and follow instructions
#
# Installs static binaries of libraries used by CRAN to build packages.
# See https://github.com/R-macos/recipes
#
# (C)2021-22 R Core Team, License: MIT, Author: Simon Urbanek

install.libs <- function(pkgs, url="https://mac.R-project.org/bin",
                         os=tolower(paste0(system("uname -s", intern=TRUE),
			   gsub("\\..*", "", system("uname -r", intern=TRUE)))),
			 arch=system("uname -m", intern=TRUE), os.arch="auto",
			 dependencies=TRUE, action=c("install", "list")) {
    up <- function(...) paste(..., sep='/')
    action <- match.arg(action)

    if (os.arch == "auto") {
        rindex <- up(url, "REPOS")
        cat("Downloading", rindex, "...\n")
        rl <- readLines(u <- url(rindex))
        close(u)
        rla <- simplify2array(strsplit(rl[grep("/", rl)], "/"))
        rl <- data.frame(os=rla[1,], arch=rla[2,])

        os.name <- function(os) gsub("[0-9].*", "", os)
        os.ver <- function(os) as.numeric(sub("\\..*", "", gsub("[^0-9]*", "", os)))

        rl$os.name <- os.name(rl$os)
        rl$os.ver <- os.ver(rl$os)

        rl <- rl[rl$os.name == os.name(os) & rl$os.ver <= os.ver(os),]
        if (nrow(rl) < 1)
            stop("There is no repository that supports ", os.name(os), " version ", os.ver(os), " or higher.\nAvailable binaries only support: ",
	         paste(rla[1,], collapse=", "))

        if (!any(rl$arch == arch))
            stop("Architecture ", arch, " is not supported on os ", os,", only available architectures: ", rl$arch)

        rl <- rl[rl$arch == arch,]
        rl <- rl[order(rl$os.ver, decreasing=TRUE),][1,]

        os.arch <- file.path(rl$os, rl$arch)
    }

    cat("Using repository ", up(url, os.arch), "...\n")

    deps <- function(pkgs, db) {
        ## convert bare (w/o version) names to full names
        bare <- pkgs %in% db[,"Package"]
        if (any(bare))
            pkgs[bare] = rownames(db)[match(pkgs[bare], db[,"Package"])]

        ## any missing?
        mis <- ! pkgs %in% rownames(db)
        if (any(mis)) stop("Following binaries have no download candidates: ", paste(pkgs[mis], ", "))

        dep <- function(pkgs) {
            nd <- na.omit(unique(c(pkgs, unlist(strsplit(db[pkgs, "BuiltWith"],"[, ]+")))))
            if (length(unique(pkgs)) < length(nd)) dep(nd) else nd
        }
        if (dependencies) dep(pkgs) else pkgs
    }

    pindex <- up(url, os.arch, "PACKAGES")
    cat("Downloading index ", pindex, "...\n")
    db <- read.dcf(u <- url(pindex))
    close(u)
    rownames(db) <- paste(db[,"Package"], db[,"Version"], sep='-')

    need <- deps(pkgs, db)
    urls <- up(url, os.arch, db[need, "Binary"])

    if (action == "install") for (u in urls) {
        cat("Downloading + installing ", u, "...\n")
        if (system(paste("curl", "-sSL", shQuote(u), "|", "tar fxj - -C /")) < 0)
            stop("Failed to install from ", u)
    } else urls
}

cat("\n Usage: install.libs(names)\n\n Example: install.libs('cairo')\n\n names can be a vector.\n See args(install.libs) for defaults.\n\n")
