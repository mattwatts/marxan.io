# marxan.io Run Marxan

cat(paste0("hello\n"))
cat(paste0(getwd(),"\n"))

sAppDir <<- getwd()

sShinyPath <<- "/mnt/shiny/"
sShinyUserPath <<- paste0(sShinyPath,"users/")
sShinyDataPath <- paste0(sShinyPath,"data/")
sShinyTempPath <<- paste0(sShinyPath,"temp/")

iRevision <- 47
sUserGuide <<- "Marxan_io_rev39_user_guide.pdf"

sMarxanApp <<- paste0("revision",iRevision,"/marxan")

if (.Platform$pkgType == "source")
{
  sShinySourcePath <<- paste0(sShinyPath,"apps/revision",iRevision,"/source/")
} else {
  sShinySourcePath <<- paste0("/Users/matt/Documents/R/revision",iRevision,"/source/")
}

sDatabase <<- ""
sMarxanDir <<- ""
irefreshinput <<- 0
irefreshmrun <<- 0
isavetargetspf <<- 0
sSampleMarxanDataset <<- "Tasmania"
sSelectDb <<- sSampleMarxanDataset
sBLM <<- 0.1
sProp <<- 0.3
sSPF <<- 1
iSpecDatRows <<- 0
iCores <<- 10
iRepsPerCore <<- 10

irefreshmap <<- 0
irefreshcluster <<- 0
irefreshtable <<- 0

sdisplaywhat <<- "map"
iAspectX <<- 1
iAspectY <<- 1
fZoomToLimits <<- TRUE
fLeafletRdata <<- FALSE
fEnableLeaflet <<- FALSE
fEnableMap <<- FALSE
fLeafletGenerated <<- FALSE
iZoom <<- 0
iBounds <<- 0

height_jscode <<-'
    $(document).on("shiny:connected", function(e) {
        var jsHeight = window.innerHeight;
        Shiny.onInputChange("GetScreenHeight",jsHeight);
    });
    '

width_jscode <<-'
    $(document).on("shiny:connected", function(e) {
        var jsWidth = window.innerWidth;
        Shiny.onInputChange("GetScreenWidth",jsWidth);
    });
    '

resize_jscode <<- '
    function resizedw(){
        var jsWidth = window.innerWidth;
        var jsHeight = window.innerHeight;
        var obj = {width: jsWidth, height: jsHeight};
        Shiny.onInputChange("GetResize", obj);
    }

    var doit;
    window.onresize = function(){
      clearTimeout(doit);
      doit = setTimeout(resizedw, 5000);
    };
    '

iHeight <<- 600
iWidth <<- 600
iAspectHeight <<- 600
iAspectWidth <<- 600
iRefreshAspectHeight <<- 600
iRefreshAspectWidth <<- 600
fLeafletHidden <<- TRUE
fMarxanHidden <<- TRUE
irefreshaspectwidth <<- 0
irefreshaspectheight <<- 0
irefreshscreenheight <<- 0
irefreshmarxanmap <<- 0
