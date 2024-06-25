import h5py
import rasterio
from rasterio.mask import mask
from rasterio.transform import from_origin
import numpy as np
import matplotlib.pyplot as plt
from variables import pol_region_map

class dnb_annual:
    def __init__(self, year, dnb_types, country_polygons, country_name):
        self.year = year
        self.dnb_types = dnb_types
        self.country_polygons = country_polygons
        self.country = country_name
        if country_name == "ukr":
            self.grids = ["h20v03", "h20v04", "h21v03", "h21v04", "h22v04"]
        elif country_name == "pol":
            self.grids = ["h19v03", "h19v04", "h20v03", "h20v04"]
        self.region_names = list(country_polygons['shapeName'])
        self.dnb_data = {dnb_type: {} for dnb_type in dnb_types}
        self.dnb_flags = {dnb_type: {} for dnb_type in dnb_types}
        self.dnb_high_quality_data = {dnb_type: {} for dnb_type in dnb_types}
        self.rasters = {}
        self.rasters_hq = {}
        self.regional_images = {dnb_type: {} for dnb_type in dnb_types}
        self.regional_images_hq = {dnb_type: {} for dnb_type in dnb_types}
        self.regional_images_pad = {dnb_type: {} for dnb_type in dnb_types}
        self.regional_images_pad_hq = {dnb_type: {} for dnb_type in dnb_types}

    def get_country_polygons(self):
        return self.country_polygons

    def load_all_data(self):
        for grid in self.grids:
            file_path = f"data/raw_viirs/{self.year}_{grid}.h5"
            with h5py.File(file_path, 'r') as annual_grid_h5:
                for dnb_type in self.dnb_types:
                    # read the data
                    annual_grid_dnb = annual_grid_h5[f'HDFEOS/GRIDS/VIIRS_Grid_DNB_2d/Data Fields/{dnb_type}'][:]
                    annual_grid_dnb[annual_grid_dnb == 65535] = 0 # replace no data values with 0
                    self.dnb_data[dnb_type][grid] = annual_grid_dnb

                    # read the corresponding flag
                    annual_grid_dnb_flag = annual_grid_h5[f'HDFEOS/GRIDS/VIIRS_Grid_DNB_2d/Data Fields/{dnb_type}_Quality'][:]
                    self.dnb_flags[dnb_type][grid] = annual_grid_dnb_flag
    
    def filter_data(self):
        for dnb_type in self.dnb_types:
            for grid in self.grids:
                # Keep only good quality data
                self.dnb_high_quality_data[dnb_type][grid] = np.where(self.dnb_flags[dnb_type][grid] == 0, self.dnb_data[dnb_type][grid], 0)

    def save_rasters(self):
        # Resolution and transform
        fill_matrix = np.zeros((2400, 2400))
        if self.country == "ukr":
            xmin, ymax = 20.0, 60.0
        elif self.country == "pol":
            xmin, ymax = 10.0, 60.0
        resolution = 15 / 3600
        transform = from_origin(xmin, ymax, resolution, resolution)

        for dnb_type in self.dnb_types:
            # stack the grids
            if self.country == "ukr":
                raster_dnb = np.vstack((np.hstack((self.dnb_data[dnb_type]["h20v03"], self.dnb_data[dnb_type]["h21v03"], fill_matrix)), 
                                    np.hstack((self.dnb_data[dnb_type]["h20v04"], self.dnb_data[dnb_type]["h21v04"], self.dnb_data[dnb_type]["h22v04"]))))
            elif self.country == "pol":
                raster_dnb = np.vstack((np.hstack((self.dnb_data[dnb_type]["h19v03"], self.dnb_data[dnb_type]["h20v03"])), 
                                    np.hstack((self.dnb_data[dnb_type]["h19v04"], self.dnb_data[dnb_type]["h20v04"]))))

            # save the raster
            file_path = f"data/annual_rasters/{self.year}_{self.country}_{dnb_type}_dnb.tif"
            with rasterio.open(file_path, 'w', driver='GTiff', height=raster_dnb.shape[0], width=raster_dnb.shape[1], 
                               count=1, dtype=raster_dnb.dtype, crs='+proj=latlong', transform=transform) as dst:
                dst.write(raster_dnb, 1) 

            # stack the high quality grids
            if self.country == "ukr":
                raster_dnb_hq = np.vstack((np.hstack((self.dnb_high_quality_data[dnb_type]["h20v03"], self.dnb_high_quality_data[dnb_type]["h21v03"], fill_matrix)), 
                                        np.hstack((self.dnb_high_quality_data[dnb_type]["h20v04"], self.dnb_high_quality_data[dnb_type]["h21v04"], self.dnb_high_quality_data[dnb_type]["h22v04"]))))
            elif self.country == "pol":
                raster_dnb_hq = np.vstack((np.hstack((self.dnb_high_quality_data[dnb_type]["h19v03"], self.dnb_high_quality_data[dnb_type]["h20v03"])), 
                                        np.hstack((self.dnb_high_quality_data[dnb_type]["h19v04"], self.dnb_high_quality_data[dnb_type]["h20v04"]))))

            # save the high-quality raster
            file_path = f"data/annual_rasters/{self.year}_{self.country}_{dnb_type}_dnb_hq.tif"
            with rasterio.open(file_path, 'w', driver='GTiff', height=raster_dnb_hq.shape[0], width=raster_dnb_hq.shape[1],
                                count=1, dtype=raster_dnb_hq.dtype, crs='+proj=latlong', transform=transform) as dst:
                 dst.write(raster_dnb_hq, 1)

    def load_rasters(self):
        for dnb_type in self.dnb_types:
            # load unfiltered data
            file_path = f"data/annual_rasters/{self.year}_{self.country}_{dnb_type}_dnb.tif"
            self.rasters[dnb_type] = rasterio.open(file_path)   

            # load high quality data
            file_path = f"data/annual_rasters/{self.year}_{self.country}_{dnb_type}_dnb_hq.tif"
            self.rasters_hq[dnb_type] = rasterio.open(file_path)


    def build_regional_images(self):
        for dnb_type in self.dnb_types:

            country_polygons = self.get_country_polygons()

            # build regional unfiltered images
            raster = self.rasters[dnb_type]
            for region, geom in zip(country_polygons['shapeName'], country_polygons.geometry):
                masked, _ = mask(dataset=raster, shapes=[geom], crop=True, all_touched=True)
                self.regional_images[dnb_type][region] = masked[0]

            # build regional high quality images
            raster_hq = self.rasters_hq[dnb_type]
            for region, geom in zip(country_polygons['shapeName'], country_polygons.geometry):
                masked, _ = mask(dataset=raster_hq, shapes=[geom], crop=True, all_touched=True)
                self.regional_images_hq[dnb_type][region] = masked[0]

    
    def plot_regional_image(self, dnb_type, region, high_quality=True):
        # get the image specifications
        country_polygons = self.country_polygons
        reg_poly = country_polygons[country_polygons['shapeName'] == region]
        if high_quality:
            reg_raster = self.regional_images_pad_hq[dnb_type][region]
        else:
            reg_raster = self.regional_images_pad[dnb_type][region]
        reg_bbox = reg_poly.total_bounds

        # plot the image
        fig, ax = plt.subplots(figsize =(12 ,8))
        im = ax.imshow(reg_raster, extent = reg_bbox[[0, 2, 1, 3]], vmin=0, vmax=40, cmap="magma")
        reg_poly.boundary.plot(ax=ax, color="skyblue", linewidth=0.4)
        plt.title(f"{region} {dnb_type} {self.year}")
        plt.show()

    def add_padding(self):
        dnb_type = self.dnb_types[0]
        max_x = max(region_image.shape[0] for region_image in self.regional_images[dnb_type].values())
        max_y = max(region_image.shape[1] for region_image in self.regional_images[dnb_type].values())

        # add padding to the images to make them all the same size
        # keep the original image in the center
        for dnb_type in self.dnb_types:
            # unflitered images
            for region, region_image in self.regional_images[dnb_type].items():
                pad_x = max_x - region_image.shape[0]
                pad_y = max_y - region_image.shape[1]
                pad_x = (pad_x // 2, pad_x - pad_x // 2)
                pad_y = (pad_y // 2, pad_y - pad_y // 2)
                self.regional_images_pad[dnb_type][region] = np.pad(region_image, (pad_x, pad_y), mode='constant', constant_values=0) 

            # high quality images
            for region, region_image in self.regional_images_hq[dnb_type].items():
                pad_x = max_x - region_image.shape[0]
                pad_y = max_y - region_image.shape[1]
                pad_x = (pad_x // 2, pad_x - pad_x // 2)
                pad_y = (pad_y // 2, pad_y - pad_y // 2)
                self.regional_images_pad_hq[dnb_type][region] = np.pad(region_image, (pad_x, pad_y), mode='constant', constant_values=0)

    def save_regional_images(self):
        for region, region_un in zip(self.region_names, map(lambda x: x.replace(" ", "_"), self.region_names)):
            # save the unfiltered images
            with h5py.File(f"data/annual_region_images/{self.year}_{region_un}.h5", 'w') as hf:
                for dnb_type in self.dnb_types:
                    hf.create_dataset(dnb_type, data=self.regional_images_pad[dnb_type][region])
            # save the high quality images
            with h5py.File(f"data/annual_region_images/{self.year}_{region_un}_hq.h5", 'w') as hf:
                for dnb_type in self.dnb_types:
                    hf.create_dataset(dnb_type, data=self.regional_images_pad_hq[dnb_type][region])

    def load_regional_images(self):
        for region, region_un in zip(self.region_names, map(lambda x: x.replace(" ", "_"), self.region_names)):
            # load the unfiltered images
            with h5py.File(f"data/annual_region_images/{self.year}_{region_un}.h5", 'r') as hf:
                for dnb_type in self.dnb_types:
                    self.regional_images_pad[dnb_type][region] = hf[dnb_type][:]
            # load the high quality images
            with h5py.File(f"data/annual_region_images/{self.year}_{region_un}_hq.h5", 'r') as hf:
                for dnb_type in self.dnb_types:
                    self.regional_images_pad_hq[dnb_type][region] = hf[dnb_type][:]

def clean_pol_polygons(pol_polygons):

    pol_polygons['name'] = pol_polygons['name'].apply(lambda x: pol_region_map[x])
    pol_polygons.rename(columns={'name': 'shapeName'}, inplace=True)

    return pol_polygons
