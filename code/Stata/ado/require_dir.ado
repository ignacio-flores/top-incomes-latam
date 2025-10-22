program define require_dir
   
    syntax , path(string)

    // Check if directory exists
    mata: st_numscalar("exists", direxists("`path'"))

    // If it doesn’t exist, create it
    if (scalar(exists) == 0) {
        mkdir "`path'"
        di as txt "Created directory: `path'"
    }
    else {
        di as txt "Directory already exists: `path'"
    }
end
