# determine directory location and structure
# bug report - mak sure that are starting in the correct directory
# need to just chop off anything after gitRepoName
dirM        <- getwd()
folders <- list.dirs(path = dirM, full.names = TRUE, recursive = TRUE)
gitRepoName <- "hydraulic.failures"
if(grepl(gitRepoName,gsub(gitRepoName,"",dirM))){
  warning('Git repository folder name is duplicated, please set working 
        directory one level up and make a new git pull.')
  dirM    <- substr(dirM,1,regexpr(gitRepoName,dirM)[[1]][1] + nchar(gitRepoName))
}

# determine if list of directories, dirsGit, needs to be created
FLAG_LIST <- ifelse(!("dirsGit" %in% ls()),
                    TRUE,
                    ifelse(!(grepl('dirM',dirsGit$dir,fixed=TRUE)),
                           TRUE,
                           FALSE))

# make list of directories if required
if(FLAG_LIST){
  dirsGit <- list(dir         = dirM,
                  Scripts     = file.path(dirM,"Scripts"),
                  Data        = file.path(dirM,"Data"),
                  ScriptsPlot = file.path(dirM,"Scripts","Plotting"),
                  Plots       = file.path(dirM,"Plots"))
}

# make folders if not present
dirsCreate <- unlist(dirsGit[2:length(dirsGit)])[!(unlist(dirsGit[2:length(dirsGit)]) %in% folders)]
if (length(dirsCreate)!=0) sapply(dirsCreate, function(d) dir.create(d))

# cleanup
rm(dirM,folders,dirsCreate,FLAG_LIST,gitRepoName)