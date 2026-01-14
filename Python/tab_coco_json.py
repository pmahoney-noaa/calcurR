import json
import pandas as pd

# Define function to convert COCO JSON to data frame
def coco_to_dataframe(coco_json_path):
    with open(coco_json_path, 'r') as f:
        coco_data = json.load(f)

    # Extract image information
    images_df = pd.DataFrame(coco_data['images'])
    images_df = images_df.rename(columns={'id': 'image_id', 'file_name': 'image_file_name'})

    # Extract annotation information
    annotations_df = pd.DataFrame(coco_data['annotations'])

    # Extract category information
    categories_df = pd.DataFrame(coco_data['categories'])
    categories_df = categories_df.rename(columns={'id': 'category_id', 'name': 'category_name'})

    # Merge dataframes to create a comprehensive tabular view
    # Start with annotations and merge image and category details
    merged_df = annotations_df.merge(images_df, on='image_id', how='left')
    merged_df = merged_df.merge(categories_df, on='category_id', how='left')

    return merged_df

# Convert and export as CSV file
df = coco_to_dataframe('../../../Desktop/seals_train_annotations.coco.json')
print(df.head())
df.to_csv('../../../Desktop/output.csv', index=False)


# Summarize instance labels by image
result = df.groupby(['image_id', 'category_name']).agg(
    count = ('category_name', 'count'),
    image_name = ('image_file_name', 'first'),
    extra = ('extra', 'first')
).reset_index()
result.to_csv('../../../Desktop/summary_output.csv', index=False)
