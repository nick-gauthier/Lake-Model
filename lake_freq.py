#!/usr/bin/env python
"""
Lake Frequency graph

"""
import sys
import csv
import grass.script as grass



def main(dem):
    levels = open('/Users/nick/gdrive/Gygaia Projects/Lake Model/depth_recon.csv')


    for i in range(1,60002)
    # Subtract the smoothed "ground" map from the original DEM
    grass.mapcalc("buildings = $dem - $ground",
                  dem = dem,ground=smooth,overwrite = True)

return 0



if __name__ == "__main__":
    options, flags = grass.parser()
    sys.exit(main(dem))
