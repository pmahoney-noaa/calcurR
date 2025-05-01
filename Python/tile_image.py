import numpy as np
from PIL import Image
from multiprocessing import Pool
import os
import math
import cv2

# Removes the cap on maximum number of pixels
Image.MAX_IMAGE_PIXELS = None

# Import image
image_path = imPath = "D:/2024_HarborSeal_Aerial_Survey/mosaics/20240614_Vandenberg-ElkhornSlough-SouthBay/20240614_break8.tif"
im = Image.open(imPath)
pixels = np.asarray(im)
pixels.shape

tile_height = 1280
tile_width = 1280

output_dir = "D:/2024_HarborSeal_Aerial_Survey/mosaics/20240614_Vandenberg-ElkhornSlough-SouthBay/"
os.makedirs(output_dir)

def tile_image(image_path, tile_width, tile_height, quiet = False, output_dir = None):
    im = Image.open(image_path)
    im_width, im_height = im.size
    name, ext = os.path.splitext(image_path)
    name = os.path.basename(name)
    
    # Output directory
    if output_dir != None:
        out_dir = output_dir + "tiles_" + str(tile_height) + "/"
        if not os.path.exists(out_dir):
            os.makedirs(out_dir)
    else:
        out_dir = "./tiles_" + str(tile_height) + "/"
    
    # Tiling breaks    
    ncols = math.ceil(im_width / tile_width)
    nrows = math.ceil(im_height / tile_height)
    
    # Run tiling
    n = 0
    for i in range(0, nrows):
        for j in range(0, ncols):
            # Extent for crop
            box = (j * tile_width, i * tile_height, j * tile_width +
                   tile_width, i * tile_height + tile_height)
            
            # Crop image
            outp = im.crop(box)
            
            # If not all black or white, export and increment
            extr = outp.convert("L").getextrema()
            if not (extr == (0, 0) or extr == (1, 1)):
                outp_path = name + "_tile_" + str(n) + ext
                outp_path = os.path.join(out_dir, outp_path)
                if not quiet:
                    print("Exporting image tile: " + outp_path)
                    print("Pixel coordinates: " + str(box))
                outp.save(outp_path)
                n += 1

# def tile_image_parallel(image_path, tile_width, tile_height, quiet = False, output_dir = None):
#     im = Image.open(image_path)
#     im_width, im_height = im.size
#     name, ext = os.path.splitext(image_path)
#     name = os.path.basename(name)
#     
#     # Output directory
#     if output_dir != None:
#         out_dir = output_dir + "tiles_" + str(tile_height) + "/"
#         if not os.path.exists(out_dir):
#             os.makedirs(out_dir)
#     else:
#         out_dir = "./tiles_" + str(tile_height) + "/"
#     
#     # Tiling breaks    
#     ncols = math.ceil(im_width / tile_width)
#     nrows = math.ceil(im_height / tile_height)
#     
#     # Extents for cropping
#     boxes = []
#     n = 0
#     for i in range(0, nrows):
#         for j in range(0, ncols):
#             # Extent for crop
#             box = (j * tile_width, i * tile_height, j * tile_width +
#                    tile_width, i * tile_height + tile_height)
#             boxes.append(box) 
# 
#     # Run tiling
#     n = 0
#     with Pool(4) as p:
#       return p.map(tile_image, boxes)
#     
# 
#             
# def tile_image_simple(image, box, out_dir, n):
#   # Crop image
#   outi = image.crob(box)
#   
#   # If not all black or white, export and increment
#   extr = outi.convert("L").getextrema()
#   if not (extr == (0, 0) or extr == (1, 1)):
#       outp_path = name + "_tile_" + str(n) + ext
#       outp_path = os.path.join(out_dir, outp_path)
#       outp.save(outp_path)
#       n += 1

            


