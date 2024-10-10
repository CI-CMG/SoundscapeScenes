"""
Converter for generating data netCDF files from MANTA output .csv files.

"""

import csv
import datetime
import logging
import os.path

import numpy as np
import pandas as pd
import xarray as xr
from datetime import datetime, date
from netCDF4 import date2num, num2date

log = logging.getLogger('MANTA.Converter')


class MantaConvert():
    def __init__(self, file_path, metadata, cal_data, version):
        super(MantaConvert, self).__init__()

        self.converter_version = 'v.1.2.0'
        self.ds = None
        self.out_file = None
        self.manta_nc = None
        self.metadata = metadata
        self.file_path = file_path
        self.cal_data = cal_data
        self.version = version
        self.old_file = None

        # Check that the data file exists.
        if not os.path.isfile(file_path):
            raise IOError(f'File {file_path} is not in data package.')

        clean_data = self.process_csv()

        # Get timestamps from clean_data DataFrame.
        raw_time_stamps = clean_data['Timestamp']
        time_stamps = np.array([stamp.to_pydatetime() for stamp in
                                raw_time_stamps[1:]])
        # Get frequencies from DataFrame column values.
        raw_freqs = clean_data.columns.values.tolist()
        frequencies = np.array([float(x) for x in raw_freqs[2:]])

        # Get the effort seconds for the data rows.
        efforts = clean_data['0'].values[1:]

        # Get main data array from DataFrame.
        psd = clean_data.iloc[1:, 2:].to_numpy()

        # Create nc file.
        self.create_acoustic_nc(time_stamps, frequencies, efforts, psd,
                                metadata, file_path)

    def process_csv(self):
        """Read CSV file into a Pandas DataFrame and then clean data of
        duplicate rows and rows with a duplicate timestamp but with an effort
        of 1 second. These rows are a known errant output from MANTA.

        Returns:
            clean_data (pd.DataFrame): Cleaned DataFrame of .csv file data.
            None - returned when there are duplicate rows that both appear to
            have real data.
        """
        parse_dates = {"Timestamp": [0]}
        # Convert csv file to Pandas DataFrame
        raw_data = pd.read_csv(self.file_path, parse_dates=parse_dates)

        # Remove rows that are complete duplicates and log removal.
        no_dups = raw_data.drop_duplicates()
        if raw_data.shape != no_dups.shape:
            removed_num = raw_data.shape[0] - no_dups.shape[0]
            log.info(f'{removed_num} duplicate rows were removed.from'
                     f' {os.path.basename(self.file_path)}')

        # Sort by Timestamp and effort seconds (column "0" which is the column
        # name not its index)) and check for duplicate timestamp values.
        no_dups = no_dups.sort_values(by=["Timestamp", "0"])
        time_diffs = no_dups['Timestamp'].diff()
        drop_rows = []
        for index, value in enumerate(time_diffs):
            value = value.total_seconds()
            first_effort = no_dups.iloc[index - 1][1]
            second_effort = no_dups.iloc[index][1]
            if value == 0.0:
                # We have found a set of duplicate timestamps. THere is a MANTA
                # issue where a row with an effort of 1 second is followed by a
                # row with a 60-second effort. The 1-second row is an error and
                # needs to be dropped. If the effort is not 1, then there is a
                # deeper issue so throw an error.
                if first_effort == 1:
                    # This is an error and this row should be dropped.
                    drop_rows.append(index-1)
                else:
                    log.error(f'{os.path.basename(self.file_path)} has '
                              f'duplicate timestamps with differing data '
                              f'values and efforts over 1 second. '
                              f'Skipping file.')
                    raise ValueError
            elif value == 59.0:
                # This might be from a spurious 1-second effort row in
                # addition to  full 60-second effort row for the same minute.
                # Test for this and delete row if appropriate.
                if second_effort == 1 and first_effort == 60:
                    drop_rows.append(index)

        if drop_rows:
            log.info(f'The following row(s) qre 1-second errors and were '
                     f'removed {drop_rows} from '
                     f'{os.path.basename(self.file_path)}')
            clean_data = no_dups.drop(drop_rows, axis=0)
        else:
            clean_data = no_dups

        return clean_data

    @staticmethod
    def get_times(time_stamps, units='seconds since 1970-01-01T00:00:00Z'):
        """Get times from timestamps.

        Args:
            time_stamps (numpy array): Time stamp array.
            units (str): units for time conversion. seconds by default.

        Returns:
            cmd (numpy array): Times array.
        """

        cdm_time = []
        for stamp in time_stamps:
            temp_stamp = date2num(stamp, units=units)
            cdm_time.append(temp_stamp)
        return np.array(cdm_time)

    @staticmethod
    def get_dates(times, units='seconds since 1970-01-01T00:00:00Z'):
        """Get times from timestamps.

        Args:
            times (list): list of integer seconds since timestamps
            units (str): units for time conversion. seconds by default.

        Returns:
            dates (list): list of times converted to ISO timestamps.
        """

        dates = []
        for stamp in times:
            temp_stamp = num2date(stamp, units=units)
            dates.append(temp_stamp)

        return dates

    def get_manta_netcdf(self):
        """Get the MANTA netCDF_converters export file tor the data file
        being processed.

        Returns:
            nc (netCDF_converters): Open netCDF_converters file.
        """
        # Due to MANTA differences in file organization and data packaging
        # there are several possible paths. We need to try them all to find
        # the correct path.
        path_1 = self.out_file.replace('.nc', '_netCDF.nc')
        path_2 = path_1.replace('csv', 'netCDF')
        path_3 = self.out_file.replace(f'_v{self.version}.nc',
                                       f'_netCDF_v{self.version}.nc')
        path_4 = path_3.replace('csv', 'netCDF')

        paths = [path_1, path_2, path_3, path_4]
        for file_path in paths:
            if os.path.isfile(file_path):
                with xr.open_dataset(file_path) as manta_ds:
                    return manta_ds

    @staticmethod
    def time_variable_attrs(time_stamps, cdm_time):
        """Create time variables.

        Args:
            time_stamps (array): String timestamp values.
            cdm_time (array):

        Returns:
            time_stamp_var: Formatted time stamp variable
            time_var: Formatted time variable.

        """
        time_stamp_var = {
            'actual_range': (time_stamps[0].isoformat(),
                             time_stamps[-1].isoformat()),
            # 'comment': 'Start times of 1-minute bins over which '
            #            'sound pressure levels are calculated',
            'long_name': "ISO timestamp beginning each 1-minute temporal "
                         "bin",
            'standard_name': "time",
            'units': 'seconds'
        }

        time_var = {
            'actual_range': (cdm_time[0], cdm_time[-1]),
            'long_name': "Time beginning each 1-minute temporal bin",
            'standard_name': "time",
            'units': "seconds since 1970-01-01T00:00:00Z",
            'coverage_content_type': 'coordinate'
        }

        return time_stamp_var, time_var

    @staticmethod
    def static_metadata(md):
        """Static metadata information be be added ro metadata dictionary.

        Returns:
            static_metadata (dict): static metadata dictionary.
        """
        static_metadata = {
            'keywords': 'GCMD:oceans, GCMD:ocean acoustics, GCMD:ambient noise,'
                        ' intensity, GCMD:marine environment monitoring, '
                        'marine habitat, sound intensity level in water, '
                        'soundscapes',
            'keyVocabulary': "GCMD: GCMD Keywords",
            'source': "Data analysis was performed using the Making Ambient "
                      "Noise Trends Accessible (MANTA, https://bitbucket.org"
                      "/CLO-BRP/manta-wiki/wiki/Home, see Miksis-Olds et al., "
                      "2021; Martin et al., 2021a,b) standalone software "
                      f"({md['DATASET_DETAILS']['SOFTWARE_VERSION']}) "
                      f"to produce hybrid millidecade spectra of "
                      "sound levels from ocean audio recordings. To "
                      "efficiently tackle large datasets, MANTA is designed "
                      "around a parallel-processing Matlab package, Raven-X "
                      "(Dugan et. al., 2014, 2016, and 2018) that uses "
                      "ordinary multi-core computers to accelerate processing "
                      "speeds. MANTA calculates the sound pressure spectral "
                      "density (PSD) levels in units of 1 µPa^2/Hz using "
                      "Welch's Method in Matlab. The Discrete Fourier "
                      "Transform length is equal to the sample rate, a Hann "
                      "window of equal length is applied to the data and 50% "
                      "overlap is used. This results in PSD estimates of "
                      "mean-square pressure amplitude (µPa^2) with a frequency "
                      "resolution of 1 Hz and temporal resolution of 1 second. "
                      "The 120 PSD estimates from each 1-minute segment were "
                      "averaged, and the average spectrum for each minute was "
                      "further processed to a hybrid millidecade (HMD) "
                      "spectrum as dB re 1 µPa^2/Hz, as defined in Martin et "
                      "al. (2021b). Hybrid millidecades are an efficient means "
                      "of storing PSD spectra from high sample rate audio "
                      "files using 1-Hz values up to 435 Hz, then millidecade "
                      "wide PSD values up to one half of the sampling rate "
                      "(Martin et al., 2021b). The MANTA outputs for each day "
                      "are: (1) CSV of the 1 minute HMD results; (2) image of "
                      "the daily long-term spectral average based on the "
                      "1 minute HMD results, (3) image of the daily spectral "
                      "probability density with percentiles, and (4) NetCDF "
                      "containing products 2 and 3 in addition to a deployment-"
                      "level MANTA Metadata output file containing the "
                      "associated frequency-dependent calibration data used to "
                      "compute the calibrated spectrum levels.",
            'references':  'Original audio recordings are available '
                           'open-access: '
                           'https://www.ncei.noaa.gov/maps'
                           '/passive-acoustic-data/. Computation of '
                           'single-sided mean-square sound pressure '
                           'spectral density with 1 Hz resolution followed '
                           'ISO 18405 3.1.3.13 (International Standard ISO 1'
                           '8405:2017(E), Underwater Acoustics – Terminology. '
                           'Geneva: ISO). Hybrid millidecade band processing '
                           'followed Martin et al. (2021; '
                           'https://doi.org/10.1121/10.0003324)',
            'license': 'CC0-1.0',
        }

        return static_metadata

    def get_metadata(self, md):
        """Update global netCDF_converters metadata information.

        Args:
            md (dict): Dictionary containing dataset metadata.

        """
        creation_date = date.today().isoformat()
        md.update(self.static_metadata(md))
        data_quality, quality_bins = self.get_quality(md)

        # Process raw scientists, projects and sponsors.
        scientists = []
        for entry in md['SCIENTISTS']:
            scientists.append(entry['name'])
        scientists = ', '.join(scientists)

        project = ', '.join(md['PROJECT_NAME'])
        # Hack to fix ONMS_sound project name
        if project == 'ONMS':
            project = 'ONMS Sound'

        sources = []
        for entry in md['SPONSORS']:
            sources.append(entry['name'])
        sources = ', '.join(sources)

        raw_metadata = {
            'acknowledgement': '',
            'history': f'Original hybrid millidecade spectra were produced by '
                       f'{scientists}. NCEI created this single '
                       f'standards-compliant netCDF file from the MANTA '
                       f'outputs plus additional metadata from the deployment '
                       f'and overall project. Conversion was done using '
                       f'{self.converter_version} of the NCEI MANTA netCDF '
                       f'converter.',
            'citation': f'Cite as: {md["citation"]}',
            'comment': data_quality,
            'creator_name': scientists,
            'creator_role': "Principal Investigator",
            'conventions': 'COARDS, CF-1.6, ACDD-1.3',
            'publisher_email': 'pad.info@noaa.gov',
            'publisher_name': ' NOAA National Centers for Environmental '
                               'Information',
            'publisher_type': 'institution',
            'publisher_url': 'https://www.ncei.noaa.gov/products'
                             '/passive-acoustic-data',
            'date_created': creation_date,
            'id': md['DOI'],
            'product_version': f'v{md["version"]}',
            'naming_authority': 'NOAA National Centers for Environmental '
                                'Information',
            'infoUrl': 'https://ncei.noaa.gov',
            'institution': sources,
            'geospatial_bounds': f"POINT ({md['DEPLOYMENT']['DEPLOY_LAT']} "
                                 f"{md['DEPLOYMENT']['DEPLOY_LON']})",
            'time_coverage_duration': "P1D",
            'time_coverage_resolution': "P60S",
            'keywords': md["keywords"],
            'keywords_vocabulary': md["keyVocabulary"],
            'license': md['license'],
            'project': project,
            'instrument': md['INSTRUMENT_TYPE'],
            'standard_name_vocabulary': 'CF Standard Name Table v80',
            'summary': md['ABSTRACT'],
            'source': md['source'],
            'title':  md['TITLE'],
            'time_offset': f"{md['DATASET_DETAILS']['ANALYSIS_TIME_ZONE']} "
                           f"hours from UTC",
            'reference': md['references']
        }

        # Populate acknowledgement field with value from settings file or
        # remove key from raw_metadata if there is no acknowledgement
        # information.
        if 'acknowledgement' in md:
            if md['acknowledgement']:
                raw_metadata['acknowledgement'] = md['acknowledgement']
            else:
                raw_metadata.pop('acknowledgement')
        else:
            raw_metadata.pop('acknowledgement')

        keys = sorted(raw_metadata.keys())
        global_attrs = {key:raw_metadata[key] for key in keys}

        return global_attrs, quality_bins

    def get_quality(self, raw_metadata, format='%Y-%m-%dT%H:%M:%S'):
        """Convert raw data quality information into JSON string suitable for
        map viewer display.

        Args:
            raw_metadata(dict): Dictionary of dataset-level metadata.
            format (str): Timestamp strong format.

        Returns(dict): parsed quality details.

        """
        quality_list = []
        quality_bins = []

        quality_values = {'Good': 1,
                          'Unverified': 2,
                          'Compromised': 3,
                          'Unusable': 4}

        raw_quality = raw_metadata['QUALITY_DETAILS']['quality_details']
        for index, entry in enumerate(raw_quality):
            if entry['quality'] == 'Select Quality Level':
                # This is a bogus entry from PassivePacker so set to nothing.
                continue
            start = entry["start"]
            end = entry["end"]
            low_freq = entry['low_freq']
            high_freq = entry['high_freq']
            if low_freq and high_freq:
                freq_str = f'from {low_freq}Hz to {high_freq}Hz'
            else:
                freq_str = f''

            raw_channels = entry['channels']
            if raw_channels:
                if len(raw_channels) == 1:
                    chan_str = f'for channel {raw_channels}'
                else:
                    chan_str = f'for channels {raw_channels}'
            else:
                chan_str = ''

            quality = (f'Data quality: {entry["quality"]} {start} to {end}'
                       f' {chan_str} '
                       f'{freq_str}')

            details = ''
            if 'comments' in entry:
                details = entry['comments']
            if details:
                if len(details) > 4:
                    # Check length of comments string. Weed out short comments
                    # that are likely just line breaks or something else silly.
                    quality = f'{quality}. {details.strip()}'

            quality_list.append(quality)

            # Convert numbers to "seconds since" integers for time indexing
            # into quality_flag array.
            start_stamp = datetime.strptime(start, format)
            end_stamp = datetime.strptime(end, format)

            cdm_start, cdm_end = self.get_times([start_stamp, end_stamp])

            quality_bins.append({'start': cdm_start, 'end': cdm_end,
                                 'low_freq': float(low_freq),
                                 'high_freq': float(high_freq),
                                 'quality': quality_values[entry["quality"]]})

        quality_statement = '; '.join(quality_list)

        return quality_statement, quality_bins

    @staticmethod
    def build_quality(time_length, freq_length):
        """Build data quality_flag array and attributes

        Args:
            time_length (int): Length of time axis.
            freq_length (int): Length of frequency axis.

        Returns:
            quality (mupy array): Initialized data quality array with default
            values.
            quality_attrs (dict)L Dictionary of data quality attributes for
            creating xarray variable.
        """

        quality_attrs = {
            'long_name': 'Data quality flag',
            'standard_name': 'quality_flag',
            'comment': '1 = Good, 2 = Not evaluated/Unknown, '
                       '3 =  Compromised/Questionable , 4 = Unusable / Bad',
            'coverage_content_type': "qualityInformation"
        }

        quality = np.full((time_length, freq_length), fill_value=2,
                          dtype=np.int8)

        return quality, quality_attrs

    def create_acoustic_nc(self, time_stamps, frequencies, efforts, psd,
                           metadata, file_path):

        # Set file path for new netCDF_converters and get Manta export netCDF_
        # converters data an xarray.
        self.out_file = file_path.replace('.csv', '.nc')

        filename = os.path.basename(file_path)

        # Try to get metadata from MANTA export file. This file is not always
        # included so set manta_ds to None if that is the case.
        try:
            manta_ds = self.get_manta_netcdf()
        except FileNotFoundError:
            manta_ds = None
            log.warning(f'MANTA metadata export file for {filename} not '
                        f'found')

        # Get times from csv_data time_stamp and get string timestamps.
        cdm_time = self.get_times(time_stamps)
        string_times = [t.isoformat() for t in time_stamps]

        # Get time variable attribute dictionaries.
        time_stamp_attrs, time_attrs = self.time_variable_attrs(time_stamps,
                                                                cdm_time)

        # Build frequency attribute dictionary.
        freq_attrs = {'actual_range': (frequencies.min(), frequencies.max()),
                      'long_name': 'Center frequency of hybrid millidecade '
                                   'spectral bands',
                      'standard_name': "sound_frequency",
                      'units': "Hz",
                      'coverage_content_type': 'coordinate'
        }

        # Build psd data attribute dictionary.
        psd_attrs = {
            'long_name': 'Single-sided mean-square sound pressure '
                         'spectral density re 1 micropascal^2/Hz',
            'standard_name': 'sound_intensity_in_water',
            'units': 'dB',
            'comment': 'Computation of single-sided mean-square sound '
                       'pressure spectral density followed ISO 18405 '
                       '3.1.3.13.',
            'coverage_content_type': 'physicalMeasurement'
        }

        effort_attrs = {
            'long_name': 'Duration of input data available for each 1-minute '
                         'bin',
            'units': 'seconds',
            'coverage_content_type': "qualityInformation"
        }

        # Create coordinates entry.
        coords = {'time': (['time'], cdm_time, time_attrs),
                  'frequency': (['frequency'], frequencies, freq_attrs)}

        # Get complete metadata entry.
        full_metadata, quality_bins = self.get_metadata(metadata)

        try:
            full_metadata.update({
                'PreampFixedGain_dB': manta_ds.PreampFixedGain_dB.item(0),
                'SamplingRate': int(manta_ds.SamplingRate.item(0)),
                'CalibrationFrequency_Hz': manta_ds.CalibrationFrequency_Hz.
                item(0),
                'CalibrationSensitivity_dB_re_1VperRefPress': manta_ds.
                CalibrationSensitivity_dB_re_1VperRefPress.item(0),
                'CalibrationDate': manta_ds.CalibrationDate.item(0).decode()
            })
        except Exception as e:
            log.debug(f'Error "{e}" getting calibration data from MANTA '
                      f'export netCDF '
                      f'{self.out_file.replace(".nc", "_netCDF.nc")}')

        # Build quality_flag data array and attributes
        quality, quality_attrs = self.build_quality(cdm_time.shape[0],
                                                    frequencies.shape[0])

        # Build data variable dictionary for dataset.
        data_vars = {'timestamp': (['time'], string_times, time_stamp_attrs),
                     'effort': (['time'], efforts, effort_attrs),
                     'psd': (['time', 'frequency'], psd, psd_attrs),
                     'quality_flag': (['time', 'frequency'], quality,
                                      quality_attrs)
                     }

        new_ds = xr.Dataset(data_vars=data_vars,
                            coords=coords,
                            attrs=full_metadata)

        # Copy calibration data into new_ds.
        new_ds['analog_sensitivity'] = self.cal_data['analog_sensitivity']
        new_ds['preamp_gain'] = self.cal_data['preamp_gain']
        new_ds['recorder_gain'] = self.cal_data['recorder_gain']
        new_ds['sensor_sensitivity'] = self.cal_data['sensor_sensitivity']

        # Set  custom _FillValue encoding to suppress the
        # automatic _FillValue attribute xarray adds when writing to netCDF.
        no_fill = {'_FillValue': None}
        encoding = {var: no_fill for var in new_ds.data_vars}
        encoding.update({var: no_fill for var in new_ds.coords})

        # Update quality_flag values.
        message = ''
        errors = set()
        for bin in quality_bins:
            # Test time ranges to confirm quality range.
            times = [bin['start'], bin['end'], new_ds.time[0],  new_ds.time[-1]]
            quality_start, quality_end, time_start, time_end = self.get_dates(
                                                                          times)
            if time_end < quality_start or time_start > quality_end:
                # This file's time range is completely outside the bin
                # so skip to next bin.
                continue
            elif time_start < quality_start:
                message = (f'{self.out_file} start {time_start} before quality '
                           f'start {quality_start}')
            if quality_end < time_end:
                message = message + (f'{self.out_file} end {time_end} after '
                                     f'quality end {quality_end}')

            if message:
                log.info(message)

            try:
                new_ds.quality_flag.loc[
                              bin['start']: bin['end'],
                              bin['low_freq']:bin['high_freq']] = bin['quality']
            except Exception as e:
                error_text = (f'Error "{e}" updating quality flag info for '
                              f'{self.out_file}')
                errors.update([error_text])

        if errors:
            for error in errors:
                log.critical(error)

        # If the version number is greater than 1 update the filename with
        # the version number. Also check if a previous version is in the
        # checksum manifest dictionary and remove it.
        version = self.metadata['version']
        if version > 1:
            if os.path.isfile(self.out_file):
                self.old_file = self.out_file

            base_path = os.path.splitext(file_path)[0]
            self.out_file = f'{base_path}_v{version}.nc'

        # Write netCDF_converters file.
        new_ds.to_netcdf(self.out_file, encoding=encoding)



