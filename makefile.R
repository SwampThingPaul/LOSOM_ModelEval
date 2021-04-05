
## makefile.R
### knits and moves files to folder for online sharing

list.files(full.names=T)
files=c("./libs/","./Plots/","./SCCF_iter1_LOSOM.html","./SCCF_iter1_LOSOM_ENLMrslt.Rmd")

webpage.loc="c:/Julian_LaCie/_GitHub/sccf-tech.github.io/slides/LOSOM"
# Folder.Maker(paste0(webpage.loc,"/iter1/"))
file.copy(files,paste0(webpage.loc,"/iter1"),overwrite=T,recursive=T)
