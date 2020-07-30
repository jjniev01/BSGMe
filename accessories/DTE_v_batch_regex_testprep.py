##  Distance to Edge (DTE)  ##
from __future__ import print_function
print("Importing packages...")
import os, arcpy, re, glob

print("Setting general paths and settings...")
#####
##  USER DEFINED PARAMETERS
##  Set the list of isos which we want to go through:
##  http://www.nationsonline.org/oneworld/country_code_list.htm
#isos = ["PER","KHM","VNM","NPL","MDG","NGA","CRI","PAK"]
isos = ["VNM"]
##  Path to the folder where your input files are placed:
#root = "E:/Research/BSGMGeneral/ValidationData/ESA CCI ANNUAL CLS 190/"
##    This is for the BSGMe:
root = "E:\\Research\\BSGMeGeneral\\data\\extents\\"

##  Name the output directory:
out_dir = "E:\\Research\\BSGMeGeneral\\data\\dte\\"

arcpy.env.overwriteOutput = True

##  Declare if this is a maupp processing run
maupp = False
##  Declare if this is an ESA processing run:
esa = False
##  This is a place holder which is autmatically filled if the maupp option is
##  on. However this is needed to avoid an error if not using the maupp option.
city = ""
print("Defining Projections...")
##  Define the projection dictionary which uses the ISO code as the key:
prj_dict = {
    "AFG":"PROJCS['WGS_1984_UTM_Zone_41.5N',GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Transverse_Mercator'],PARAMETER['False_Easting',500000.0],PARAMETER['False_Northing',0.0],PARAMETER['Central_Meridian',66.0],PARAMETER['Scale_Factor',0.9996],PARAMETER['Latitude_Of_Origin',0.0],UNIT['Meter',1.0]]",
    "VNM":"PROJCS['WGS_1984_UTM_Zone_48N',GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Transverse_Mercator'],PARAMETER['False_Easting',500000.0],PARAMETER['False_Northing',0.0],PARAMETER['Central_Meridian',105.0],PARAMETER['Scale_Factor',0.9996],PARAMETER['Latitude_Of_Origin',0.0],UNIT['Meter',1.0]]",
    "KEN":"PROJCS['WGS_1984_UTM_Zone_37N',GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Transverse_Mercator'],PARAMETER['False_Easting',500000.0],PARAMETER['False_Northing',0.0],PARAMETER['Central_Meridian',39.0],PARAMETER['Scale_Factor',0.9996],PARAMETER['Latitude_Of_Origin',0.0],UNIT['Meter',1.0]]",
    "CRI":"PROJCS['WGS_1984_UTM_Zone_16.5N',GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Transverse_Mercator'],PARAMETER['False_Easting',500000.0],PARAMETER['False_Northing',0.0],PARAMETER['Central_Meridian',-84.0],PARAMETER['Scale_Factor',0.9996],PARAMETER['Latitude_Of_Origin',0.0],UNIT['Meter',1.0],AUTHORITY['EPSG',32616]]",
    "HTI":"PROJCS['WGS_1984_UTM_Zone_18.5N',GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Transverse_Mercator'],PARAMETER['False_Easting',500000.0],PARAMETER['False_Northing',0.0],PARAMETER['Central_Meridian',-72.0],PARAMETER['Scale_Factor',0.9996],PARAMETER['Latitude_Of_Origin',0.0],UNIT['Meter',1.0],AUTHORITY['EPSG',32618]]",
    "NAM":"PROJCS['WGS_1984_UTM_Zone_33.5S',GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Transverse_Mercator'],PARAMETER['False_Easting',500000.0],PARAMETER['False_Northing',0.0],PARAMETER['Central_Meridian',18.0],PARAMETER['Scale_Factor',0.9996],PARAMETER['Latitude_Of_Origin',0.0],UNIT['Meter',1.0]]",
    "THA":"PROJCS['WGS_1984_UTM_Zone_46.5N',GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Transverse_Mercator'],PARAMETER['False_Easting',500000.0],PARAMETER['False_Northing',0.0],PARAMETER['Central_Meridian',96.0],PARAMETER['Scale_Factor',0.9996],PARAMETER['Latitude_Of_Origin',0.0],UNIT['Meter',1.0]]",
    "MMR":"PROJCS['WGS_1984_UTM_Zone_46.5N',GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Transverse_Mercator'],PARAMETER['False_Easting',500000.0],PARAMETER['False_Northing',0.0],PARAMETER['Central_Meridian',96.0],PARAMETER['Scale_Factor',0.9996],PARAMETER['Latitude_Of_Origin',0.0],UNIT['Meter',1.0]]",
    "NPL":"PROJCS['WGS_1984_UTM_Zone_44.5N',GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Transverse_Mercator'],PARAMETER['False_Easting',500000.0],PARAMETER['False_Northing',0.0],PARAMETER['Central_Meridian',84.0],PARAMETER['Scale_Factor',0.9996],PARAMETER['Latitude_Of_Origin',0.0],UNIT['Meter',1.0]]",
    "PAK":"PROJCS['WGS_1984_UTM_Zone_42N',GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Transverse_Mercator'],PARAMETER['False_Easting',500000.0],PARAMETER['False_Northing',0.0],PARAMETER['Central_Meridian',69.0],PARAMETER['Scale_Factor',0.9996],PARAMETER['Latitude_Of_Origin',0.0],UNIT['Meter',1.0]]",
    "MEX":"PROJCS['WGS_1984_UTM_Zone_13.5N',GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Transverse_Mercator'],PARAMETER['False_Easting',500000.0],PARAMETER['False_Northing',0.0],PARAMETER['Central_Meridian',-102.0],PARAMETER['Scale_Factor',0.9996],PARAMETER['Latitude_Of_Origin',0.0],UNIT['Meter',1.0]]",
    "ECU":"PROJCS['WGS_1984_UTM_Zone_17S',GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Transverse_Mercator'],PARAMETER['False_Easting',500000.0],PARAMETER['False_Northing',0.0],PARAMETER['Central_Meridian',-81.0],PARAMETER['Scale_Factor',0.9996],PARAMETER['Latitude_Of_Origin',0.0],UNIT['Meter',1.0]]",
    "TZA":"PROJCS['WGS_1984_UTM_Zone_36.5S',GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Transverse_Mercator'],PARAMETER['False_Easting',500000.0],PARAMETER['False_Northing',0.0],PARAMETER['Central_Meridian',36.0],PARAMETER['Scale_Factor',0.9996],PARAMETER['Latitude_Of_Origin',0.0],UNIT['Meter',1.0]]",
    "NGA":"PROJCS['WGS_1984_UTM_Zone_32N',GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Transverse_Mercator'],PARAMETER['False_Easting',500000.0],PARAMETER['False_Northing',0.0],PARAMETER['Central_Meridian',9.0],PARAMETER['Scale_Factor',0.9996],PARAMETER['Latitude_Of_Origin',0.0],UNIT['Meter',1.0]]",
    "MDG":"PROJCS['WGS_1984_UTM_Zone_38.5S',GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Transverse_Mercator'],PARAMETER['False_Easting',500000.0],PARAMETER['False_Northing',0.0],PARAMETER['Central_Meridian',48.0],PARAMETER['Scale_Factor',0.9996],PARAMETER['Latitude_Of_Origin',0.0],UNIT['Meter',1.0]]",
    "PER":"PROJCS['WGS_1984_UTM_Zone_18S',GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Transverse_Mercator'],PARAMETER['False_Easting',500000.0],PARAMETER['False_Northing',0.0],PARAMETER['Central_Meridian',-75.0],PARAMETER['Scale_Factor',0.9996],PARAMETER['Latitude_Of_Origin',0.0],UNIT['Meter',1.0]]",
    "KHM":"PROJCS['WGS_1984_UTM_Zone_48N',GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Transverse_Mercator'],PARAMETER['False_Easting',500000.0],PARAMETER['False_Northing',0.0],PARAMETER['Central_Meridian',105.0],PARAMETER['Scale_Factor',0.9996],PARAMETER['Latitude_Of_Origin',0.0],UNIT['Meter',1.0]]"}

##  In practice nothing below this line should need to be changed  ##
##  Using regular expressions, find all files matching the pattern:
print("Constructing regular expression...")
##  Create an or statement within the regex based upon the isos:
if len(isos) == 1:
    i = isos[0]
elif len(isos) >1:
    i="("
    length = len(isos)
    for n,c in enumerate(isos):
        if n != (length-1):
            i += c+"|"
        elif n == (length-1):
            i=i+c+")"


##  Fully construct the regex:
regq = ".*(BSGM_Extentsprj_\d{4}-\d{4}_"+i+"_\d{4}[.]tif$)"
if esa:
    regq = ".*("+i+"_ESA_Binary_190_\d{4}.tif$)"
if maupp:
    regq = ".*(MAUPP_[A-Z]{3}_.*_classes_\d{4}[.]shp$)"
print("Regular expression: {0}".format(regq))
##  Compile it for a list comprehension:
regqc = re.compile(regq)

print("Recursively retrieving a list of all tif files in the root directory...")
##  Retrieve, recursively ("**"), all tif files from the root folder:
#flist = glob.glob(root+"/**/*.tif")
##  Non recursive version:
flist = glob.glob(root+"/*.tif")
if esa:
  flist = glob.glob(root + "/*.tif")
if maupp:
  flist = glob.glob(root + "/*.shp")
##  Subset that file list for only the tifs matching our regular expression
##  query using a list comprehension
##  (http://www.cademuir.eu/blog/2011/10/20/python-searching-for-a-string-within-a-list-list-comprehension/):
bsextents_paths = [m.group(0) for f in flist for m in [regqc.search(f)] if m]
bsextents_fn = [m.group(1) for f in flist for m in [regqc.search(f)] if m]
print("All retrieved files: ")
print(bsextents_fn)
print("Checking Out Extentsions...")
##  Check out the Spatial Analyst Extension:
arcpy.CheckOutExtension("Spatial")
from arcpy.sa import *

##  Define a function that replicates the Con() in Arcmap so I can do Raster Math without the
##  excessive ifelse and the string formatting needed for raster calculator:
##def Conditional(condition, trueValue, falseValue):
##    if condition:
##        foo = 1
##        opp = 0
##    else:
##        foo = 0
##        opp = 1
##
##    return(foo * trueValue + (opp)*falseValue)
print("Defining Functions...")
##	Create a directory function to check for and create data directories if they don't exist:
def ensure_dir(d):
	if not os.path.isdir(d):
		os.makedirs(d)
	return d

##  Define the function that actually calculates the distance to edge:
def DTE(tiffile,iso,in_file, tag=""):
    '''
    tiffile is the full path to the input raster of interest
    iso is the three letter string representing the country of interest
    in_file is the file name of the original .tif (including the .tif
    '''
    print("Working on {0} {1}".format(iso,tag))
    if maupp == False:
        print("\tProjecting Data...")
        ##  Project the dataset  ##
        ##    Retrieve projection from dictionary:
        intermediate_prj = prj_dict[iso]

        ##    Set the ouput data name and temporary folder:
        tmppath = ensure_dir(out_dir+"/tmp/")
        output =  tmppath + in_file.split('.tif')[0] + "_projected.tif"

        ##    Project the dataset:
        data_desc = arcpy.Describe(tiffile)
        input_prj = data_desc.SpatialReference.exportToString()
        arcpy.ProjectRaster_management(tiffile, output, intermediate_prj, "NEAREST","100","#","#",input_prj)

        ##  Retrieve the projected dataset:
        prjtif = output

        print("\tPolygonizing...")
        ##  Calculate the DTE  ##
        ##    Raster to Polygon:
        arcpy.RasterToPolygon_conversion(prjtif, tmppath + iso + "_polyras.shp","NO_SIMPLIFY","VALUE")
        polyras = tmppath + iso + "_polyras.shp"
    if maupp:
        urbtif = os.path.dirname(tiffile)+"\\Derived\\"+os.path.basename(tiffile).rstrip(".shp")+".tif"
        ##  Project the dataset  ##
        ##    Retrieve projection from dictionary:
        intermediate_prj = prj_dict[iso]
        ##  Project the raster version:
        tmppath = ensure_dir(out_dir+"/tmp/")
        output =  tmppath + os.path.basename(urbtif).split('.tif')[0] + "_projected.tif"
        ##    Project the dataset:
        data_desc = arcpy.Describe(urbtif)
        input_prj = data_desc.SpatialReference.exportToString()
        arcpy.ProjectRaster_management(urbtif, output, intermediate_prj, "NEAREST","100","#","#",input_prj)
        ##  Retrieve the projected dataset:
        urbtif = output

        ##  Project the shapefile:
        ##    Set the output data name and temporary folder:
        tmppath = ensure_dir(out_dir+"/tmp/")
        inshp = tiffile
        ##  Project that shapefile:
        output = polyras = tmppath + iso + "_polyras.shp"
        arcpy.Project_management(inshp, output, intermediate_prj,preserve_shape = "PRESERVE_SHAPE")
        polyras = output

    ##    Convert the polygon to a feature layer:
    polyfeat = arcpy.MakeFeatureLayer_management(polyras, "polyras")

    print("\tLinizing...")
    ##    Polygon to Line:
    arcpy.PolygonToLine_management(polyfeat, tmppath + iso + "_polyline.shp", "IGNORE_NEIGHBORS")
    polyline = tmppath + iso + "_polyline.shp"

    print("\tCalculating Distance to Line...")
    ##    Distance to Line:
    output = tmppath + iso + "dst.tif"
    dstras = arcpy.sa.EucDistance(polyline, cell_size = 100)
    dstras.save(output)


    ##    Convert the internal numbers to negative using rastermath:
    print("\tCalculating negative values...")
    outpath = ensure_dir(out_dir)
    output = tmppath + iso + "_projected_DTE.tif"
    if maupp == False:
        urb = arcpy.Raster(tiffile)
    if maupp:
        urb = arcpy.Raster(urbtif)
    dteras = arcpy.sa.Con(urb == 1,-1,0) * dstras + arcpy.sa.Con(urb == 0,1,0)*dstras
    dteras.save(output)

    ##  Reproject back to WGS 84 using the original as the snap raster:
    ##    Retrieve the info we want out of the file name:
    reg = ".*([A-Z]{3}_[0-9]{4})[.]tif"
    if esa:
        reg = ".*(\d{4})[.]tif"
    if maupp:
        reg="(MAUPP_[A-Z]{3}).*[.]shp"
    m = re.search(reg,in_file)
    ##    Construct output file path:
    output = out_dir + m.group(1) + tag + '_DTE_WGS84.tif'
    if esa:
        output = out_dir + i + "_" + m.group(1) + tag + '_DTE_WGS84.tif'

    ##    Declare snap raster:
    #arcpy.env.snapRaster = arcpy.Raster(tiffile)
    arcpy.ProjectRaster_management(dteras, output, input_prj, 'BILINEAR',"0.0008333","#","#",intermediate_prj)



##  END FUNCTION DECLARATIONS
#####




#####
##  BEGIN PROCESSING
##
##  For every retrieved file we wanted to run things on:
for t in range(0,len(bsextents_paths)):
    ##  Retrieve the iso from the file name:
    isoreg = ".*_([A-Z]{3})_[0-9]{4}[.]tif"
    tag = ""
    if esa:
        isoreg = ".*([A-Z]{3})_ESA_Binary.*[.]tif"
        tag = "ESA"
    if maupp:
        isoreg = ".*_([A-Z]{3})_.*_classes_\d{4}[.]shp"
    im =re.search(isoreg,bsextents_fn[t])
    iso_name = im.group(1)
    if maupp:
        ##  Retrieve the city name:
        cityreg =".*_[A-Z]{3}_(.*)_classes_(\d{4})[.]shp"
        cm = re.search(cityreg, bsextents_fn[t])
        city = cm.group(1)+cm.group(2)
        tag = city

    print("Processing {0}\n    Using file: {1}".format(iso_name, bsextents_fn[t]))
    DTE(bsextents_paths[t],iso_name,bsextents_fn[t], tag = tag)

print("Checking In Extension")
arcpy.CheckInExtension("Spatial")


