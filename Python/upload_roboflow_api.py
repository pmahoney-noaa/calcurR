# To run in R if required
# library(reticulate)
# py_require("dotenv")
# py_require("roboflow")

from roboflow import Roboflow
from dotenv import load_dotenv
import os

# Required functions
def list_files_in_directory(directory_path, full_path_name = True):
    """
    Lists all files (excluding subdirectories) in a given directory.

    Args:
        directory_path (str): The path to the directory.
        full_path_name (logical) : whether to return full or partial file name.

    Returns:
        list: A list of filenames in the specified directory.
    """
    
    files = []
    for entry in os.listdir(directory_path):
      full_path = os.path.join(directory_path, entry)
      if os.path.isdir(full_path):
        for image in os.listdir(full_path):
          full_image_path = os.path.join(full_path, image)
          if full_path_name:
                files.append(full_image_path)
          else:
                files.append(image)
    return files

# Load API key
load_dotenv()  # Load variables from .env file
api_key = os.getenv("ROBOFLOW_API_KEY")

# Initialize the Roboflow object with your API key
rf = Roboflow(api_key = api_key)

# Retrieve your current workspace and project name
print(rf.workspace())

# Specify the project for upload
# let's you have a project at https://app.roboflow.com/my-workspace/my-project
workspaceId = 'noaa-workspace'
projectId = 'seals-lgx79'
project = rf.workspace(workspaceId).project(projectId)

# Find files to upload
image_dir = "D:/2024_HarborSealMosaics/20240519_SanNicolas/"
files = list_files_in_directory(image_dir, full_path_name = True)

# Upload the image to your project
for fi in files[1067:len(files)]:
  print("Uploading image tile: " + fi)
  project.upload(
    fi, 
    batch_name = "20240519_SanNicolas",
    num_retry_uploads=10
    )



