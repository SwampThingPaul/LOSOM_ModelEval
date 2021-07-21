
## makefile.R
### knits and moves files to folder for online sharing
# Converts HTML slides to pdf
Sys.setenv(PAGEDOWN_CHROME="C:/Users/Paul Julian/AppData/Local/Google/Chrome/Application/chrome.exe")
# pagedown::chrome_print("SCCF_iter2_TechWorkshop.Rmd")
# pagedown::chrome_print("SCCF_iter2_TechWorkshopClimate.Rmd")
# pagedown::chrome_print("SCCF_iter2_LOSOM_ENLMrslt.Rmd")
# pagedown::chrome_print("SCCF_iter2_LOSOM_LakeBasinLoad.Rmd")
# pagedown::chrome_print("SCCF_iter2_LOSOM_SR35.Rmd")

## Move files
list.files(full.names=T)
files=c("./libs/","./Plots/","./SCCF_iter1_LOSOM.html","./SCCF_iter1_LOSOM_ENLMrslt.html","./pareto_reeval.html","./sccf_iter2_pareto.html")
files=c(files,"./SCCF_iter2_LOSOM.html")
files=c(files,"./SCCF_iter2_TechWorkshop.html","./SCCF_iter2_TechWorkshop.pdf")
files=c(files,"./SCCF_iter2_TechWorkshopClimate.html","./SCCF_iter2_TechWorkshopClimate.pdf")
files=c(files,"./sccf_iter2_summary.html")
files=c(files,"./SCCF_iter2_LOSOM_ENLMrslt.html","./SCCF_iter2_LOSOM_ENLMrslt.pdf")
files=c(files,"./SCCF_iter2_LOSOM_LakeBasinLoad.html","./SCCF_iter2_LOSOM_LakeBasinLoad.pdf")
files=c(files,"./SCCF_iter2_LOSOM_SR35.html","./SCCF_iter2_LOSOM_SR35.pdf")

webpage.loc="c:/Julian_LaCie/_GitHub/sccf-tech.github.io/slides/LOSOM"
# Folder.Maker(paste0(webpage.loc,"/iter1/"))
file.copy(files,webpage.loc,overwrite=T,recursive=T)

# files=c("./DataReports/Iteration1_stagecurve.html","./DataReports/footer.html")
# webpage.loc="c:/Julian_LaCie/_GitHub/sccf-tech.github.io/DataReports/LOSOM"
# file.copy(files,webpage.loc,overwrite=T,recursive=T)
#