
import os
import logging
import datetime

import pandas as pd

from converters.manta_convert import MantaConvert
from common_tools import utilities as ut
from pad_tools.netCDF_converters.base_converter import BaseConverter

log = logging.getLogger('MANTA')


class MantaNetCDF(BaseConverter):
    def __init__(self):
        """
        Class to automate the conversion product .csv files packaged by
        PassivePacker into to netCDF_converters and add file to packages
        manifest file.

        """
        super().__init__()

        # Get path to directory containing CSV files to process.
        base_path = ut.get_path(self.starting_path)
        directory = os.path.basename(base_path)

        ut.reset_logging(log, path=self.log_base,
                         file_name=F'{directory}-convert',
                         file_level=self.log_level,
                         screen_print=True, screen_level='info')

        start = datetime.datetime.now()

        # Get a list of potential package directories in the base path.
        root, dirs, ignore = next(os.walk(base_path))

        count = 0
        errors = 0
        for index, package in enumerate(dirs):
            # if index > 0:
            #     continue
            log.info(f'Processing {package}')
            self.package_path = os.path.join(root, package)

            # Get metadata.
            try:
                metadata_file = os.path.join(self.package_path, 'data',
                                             f'{package}.json')
                if not os.path.isfile(metadata_file):
                    metadata_file = (os.path.join(self.package_path, 'data',
                                     f'{package}_v{self.version}.json'))

                self.get_metadata(metadata_file)
            except Exception as e:
                if 'No such file or directory' in str(e):
                    log.info(f'No metadata file in {package}, '
                             f'This is likely not a data package')
                else:
                    log.critical(f'Error "{e}" getting metadata for {package}, '
                                 f'Skipping package')
                continue

            # Update metadata with citation and version info from settings
            # file parsed in base converter.
            self.metadata['citation'] = self.citation
            self.metadata['version'] = self.version
            self.metadata['acknowledgement'] = self.acknowledgement
        
            # Handle DOIs. Either use DOI passed in from settings file or
            # mint a fresh DOI for this dataset.
            if self.doi_minting:
                self.mint_doi()
            else:
                self.metadata['DOI'] = self.doi

            # Parse manifest file and make copy of the original if backup=True.
            try:
                manifest_file = os.path.join(self.package_path,
                                             'manifest-md5.txt')
                self.parse_manifest(manifest_file, backup=True)
            except Exception as e:
                log.critical(
                    f'Error "{e}" Parsing manifest for {package}, skipping '
                    f'package.')
                continue

            # Get calibration and other metadata info from calibration file in
            # calibration directory.
            cal_metadata, raw_cal_data = self.parse_cal_file()

            # Update calibration data xarray.
            cal_data = self.update_cal_data(raw_cal_data)

            # Get a master list of all files in all directories and
            # subdirectories under data_files in package.
            data_dir = f'{self.package_path}/data'
            raw_files = []
            for entries in os.walk(data_dir):
                files = [os.path.join(entries[0], file) for file in entries[2]
                         if
                         '._' not in file]
                raw_files.extend(files)

            # Process package
            try:
                self.process_package(cal_data)

                # Create new manifest file with updated entries.
                self.write_manifest(manifest_file)

                log.info(f'Completed: {package}')
                count += 1
            except Exception as e:
                log.critical(f'Error "{e}" processing {package}')
                errors += 1

        # Processing complete
        total_time = datetime.datetime.now() - start
        log.info(
            f'Processing complete. {count} datasets processed in {total_time}')
        if errors:
            log.info(f'{errors} errors occurred during processing')

    def process_package(self, cal_data):
        """Process the package to create netCDF_converters file and rearrange
        files as needed.

        Args:
            cal_data (DataFrame): Pandas DataFrame containing calibration data.

        """
        dir_path = self.package_path
        manifest = self.manifest
        metadata = self.metadata

        csv_files = [key for key in manifest.keys() if '.csv' in key]

        for file in csv_files:
            file_path = os.path.join(dir_path, file)

            try:
                converter = MantaConvert(file_path, metadata, cal_data,
                                         self.version)
            except IOError as e:
                log.critical(e)
                continue
            except ValueError:
                # We've already logged the issue raising this error so just
                # skip to next file.
                continue
            except Exception as e:
                log.critical(e)
                continue
            if not converter:
                # We did not want to convert this file so pass.
                log.error(f'No converter found for {file_path}')
                continue

            # Get checksum for new netCDF_converters file and add it to the
            # manifest dictionary.
            if converter.old_file:
                # Remove old (previous version file) and pop its entry from
                # the manifest dictionary.
                os.remove(converter.old_file)
                old_path = converter.old_file.replace(f'{dir_path}/', '')
                if old_path in manifest:
                    manifest.pop(old_path)
            manifest_path = converter.out_file.replace(f'{dir_path}/', '')
            checksum = ut.get_checksum(converter.out_file)
            manifest[manifest_path] = checksum.split('  ')[0]
            log.debug(f'Created {manifest_path}')

        # Rename all non-data netcdf files in package if this is a
        # versioned dataset.
        if self.version > 1:
            self.rename_files()

    def rename_files(self):
        """Programmatically append all filenames with _v<version number> for
        datasets with a version greater than 1.

        """
        manifest = self.manifest
        new_manifest = {}
        for key in manifest.keys():
            this_path, file_name = os.path.split(key)
            file_root, extension = os.path.splitext(file_name)
            if not file_root[-2:] == f'v{self.version}':
                new_name = f'{file_root}_v{self.version}{extension}'
                log.debug(f'Renaming {file_name} to {new_name}')
                new_key = os.path.join(this_path, new_name)
                new_manifest[new_key] = manifest[key]
                old_path = os.path.join(self.package_path, key)
                if not os.path.isfile(old_path):
                    # This file is in manifest but not in the data package.
                    # Likely this is due to  a manifest thT was updated with
                    # previous version files. Skip on to the next file.
                    continue
                new_path = os.path.join(self.package_path, new_key)
                os.rename(old_path, new_path)
            else:
                # Pass Versioned files into new manifest file.
                new_manifest[key] = manifest[key]

        self.manifest = new_manifest

    def write_manifest(self, manifest_path):
        """Write updated manifest path. WARNING! calling this method will
        overwrite the original manifest file

        Args:
            manifest_path (str): Full path for output manifest file

        """
        with open(manifest_path, 'w') as m:
            for path, checksum in self.manifest.items():
                # Make sure path separators are unix style.
                path.replace('\\', '/')
                m.write(f'{checksum}  {path}\n')

    def parse_cal_file(self):
        """Get calibration and other metadata from calibration file.

        Returns:

        """
        # Get listing of files in calibration directory.
        cal_dir = os.path.join(self.package_path, 'data', 'calibration')
        if not os.path.isdir(cal_dir):
            cal_dir = os.path.join(self.package_path, 'data', 'data_files')

        root, dirs, files = ut.walk_dir(cal_dir)

        # Filter out only Excel files from file list.
        files = [file for file in files if '.xls' in file and '._' not in file]

        # Process calibration file.
        for file in files:
            cal_path = os.path.join(root, file)
            cal_metadata = pd.read_excel(cal_path, sheet_name=0)
            cal_data = pd.read_excel(cal_path, sheet_name=1)
            try:
                cal_data.set_index('Frequency_Hz', inplace=True)
                cal_data = cal_data.to_xarray()
                return cal_metadata, cal_data
            except Exception as e:
                log.info(f'Calibration file {file} had error "{e}". '
                         f'Maybe not a proper calibration metadata file? ')
                continue

    def update_cal_data(self, raw_cal_data):
        """Update calibration data xarray Dataset. Changing writable names and
        adding attributes

        Args:
            raw_cal_data (xarray): Raw calibration data xarray pulled from
            calibration Excel file.

        Returns:
            cal_data (xarray): Updated calibration data xarray Dataset

        """
        # Rename variables.
        cal_data = raw_cal_data.rename(name_dict={
                'Frequency_Hz': 'cal_frequency',
                'AnalogSensitivity_dB_re_1VperRefPress': 'analog_sensitivity',
                'PreampGain_dB': 'preamp_gain',
                'RecorderGain_dB': 'recorder_gain',
                'SensorSensitivity_dB_re_1V_perRefPress': 'sensor_sensitivity'})

        # Update attributes.
        cal_data.cal_frequency.attrs = {
                        'long_name': 'Calibration frequency for hybrid '
                                     'millidecade spectral bands',
                        'units': 'Hz'}
        cal_data.analog_sensitivity.attrs = {
                            'long_name': 'Analog sensitivity re 1 V per '
                                         'reference pressure',
                            'units': 'dB',
                            'coverage_content_type': 'physicalMeasurement',
                            'comment': 'Sensitivity values in dB measured by '
                                       'the manufacturer were linearly '
                                       'interpolated to the center '
                                       'frequencies of hybrid millidecade bands'
        }
        cal_data.sensor_sensitivity.attrs = {
                            'long_name': 'Sensor sensitivity re 1 V per '
                                         'reference pressure',
                            'units': 'dB',
                            'coverage_content_type': 'physicalMeasurement',
                            'comment': 'Sensitivity values in dB measured by '
                                       'the manufacturer were linearly '
                                       'interpolated to the center '
                                       'frequencies of HMD bands.'
        }
        cal_data.preamp_gain.attrs = {'long_name': 'Preamp gain', 'units': 'dB'}
        cal_data.recorder_gain.attrs = {'long_name': 'Recorder gain',
                                        'units': 'dB'}

        return cal_data


if __name__ == '__main__':
    MantaNetCDF()