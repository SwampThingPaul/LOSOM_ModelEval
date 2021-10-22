
## makefile.R
### knits and moves files to folder for online sharing
# Converts HTML slides to pdf
Sys.setenv(PAGEDOWN_CHROME="C:/Users/Paul Julian/AppData/Local/Google/Chrome/Application/chrome.exe")
# pagedown::chrome_print("SCCF_iter2_TechWorkshop.Rmd")
# pagedown::chrome_print("SCCF_iter2_TechWorkshopClimate.Rmd")
# pagedown::chrome_print("SCCF_iter2_LOSOM_ENLMrslt.Rmd")
# pagedown::chrome_print("SCCF_iter2_LOSOM_LakeBasinLoad.Rmd")
# pagedown::chrome_print("SCCF_iter2_LOSOM_SR35.Rmd")
# pagedown::chrome_print("SCCF_iter2_SR35_CRESum.Rmd")
# pagedown::chrome_print("SCCF_postiter2_sum.Rmd")
# pagedown::chrome_print("SCCF_postiter2_sum_opt.Rmd")
# pagedown::chrome_print("SCCF_CSWFL_briefing_20210831.Rmd")
# pagedown::chrome_print("sccf_iter3_p1.Rmd")
# pagedown::chrome_print("sccf_iter3_p1_v2.Rmd")
# pagedown::chrome_print("sccf_iter3_p1_FDEP.Rmd")
# pagedown::chrome_print("sccf_iter3_p2.Rmd")

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
files=c(files,"./SCCF_iter2_SR35_CRESum.html","./SCCF_iter2_SR35_CRESum.pdf")
files=c(files,"./SCCF_postiter2_sum.html","./SCCF_postiter2_sum.pdf")
files=c(files,"./SCCF_postiter2_sum_opt.html","./SCCF_postiter2_sum_opt.pdf")
files=c(files,"./SCCF_CSWFL_briefing_20210831.html","./SCCF_CSWFL_briefing_20210831.pdf")
files=c(files,"./sccf_iter3_p1.html","./sccf_iter3_p1.pdf")
files=c(files,"./sccf_iter3_p1_v2.html","./sccf_iter3_p1_v2.pdf")
files=c(files,"./sccf_iter3_p1_FDEP.html","./sccf_iter3_p1_FDEP.pdf")
files=c(files,"./sccf_iter3_p2.html","./sccf_iter3_p2.pdf")

webpage.loc="c:/Julian_LaCie/_GitHub/sccf-tech.github.io/slides/LOSOM"
# Folder.Maker(paste0(webpage.loc,"/iter1/"))
file.copy(files,webpage.loc,overwrite=T,recursive=T)

# files=c("./DataReports/Iteration1_stagecurve.html","./DataReports/footer.html")
# webpage.loc="c:/Julian_LaCie/_GitHub/sccf-tech.github.io/DataReports/LOSOM"
# file.copy(files,webpage.loc,overwrite=T,recursive=T)
#