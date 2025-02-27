from ctypes import *
import numpy as np

def get_temp_dirp2(
            filepath_image: str,
            image_height: int,
            image_width: int,
            object_distance: float,
            relative_humidity: float,
            emissivity: float,
            reflected_apparent_temperature: float,
            filepath_dll: str,
    ):
        """
        Calculates the temperature based on the input variables.
        `dirp2` means `DJI IR Processing Version 2nd`.

        Args:
            filepath_image: str, relative path of R-JPEG image
            image_height: float, image height
            image_width: float, image width
            object_distance: float, The distance to the target. Value range is [1~25] meters.
            relative_humidity: float, The relative humidity of the environment. Value range is [20~100] percent. Defualt value is 70%.
            emissivity: float, How strongly the target surface is emitting energy as thermal radiation. Value range is [0.10~1.00].
            reflected_apparent_temperature: float, Reflected temperature in Celsius. The surface of the target that is measured could reflect the energy radiated by the surrounding objects. Value range is [-40.0~500.0]
            filepath_export: str, path to where the temperature values will be stored as .txt file, str should end in .../filename.txt
            filepath_dll: str, path to where the dynamic link libarary is stored

        Retruns:
            str: Succes

        Exports:
            a .txt file containing a list with the calculated temperature values

        References:
            * [DJI Thermal SDK](https://www.dji.com/cn/downloads/softwares/dji-thermal-sdk)
        """
                
        DIRP_HANDLE = c_void_p
        DIRP_VERBOSE_LEVEL_NONE = 0  # 0: Print none
        DIRP_VERBOSE_LEVEL_DEBUG = 1  # 1: Print debug log
        DIRP_VERBOSE_LEVEL_DETAIL = 2  # 2: Print all log
        DIRP_VERBOSE_LEVEL_NUM = 3  # 3: Total number

        dtype = np.float32
        _dtype = dtype

        class dirp_rjpeg_version_t(Structure):
            """
            References:
                * [DJI Thermal SDK](https://www.dji.com/cn/downloads/softwares/dji-thermal-sdk)
            """

            _fields_ = [
                # Version number of the opened R-JPEG it
                ('rjpeg', c_uint32),
                # Version number of the header data in R-JPEG
                ('header', c_uint32),
                # Version number of the curve LUT data in R-JPEG
                ('curve', c_uint32),
            ]

        class dirp_resolution_t(Structure):
            """
            References:
                * [DJI Thermal SDK](https://www.dji.com/cn/downloads/softwares/dji-thermal-sdk)
            """

            _fields_ = [
                # Horizontal size
                ('width', c_uint32),
                # Vertical size
                ('height', c_uint32),
            ]

        class dirp_measurement_params_t(Structure):
            """
            References:
                * [DJI Thermal SDK](https://www.dji.com/cn/downloads/softwares/dji-thermal-sdk)
            """

            _fields_ = [
                # The distance to the target. Value range is [1~25] meters.
                ('distance', c_float),
                # How strongly the target surface is emitting energy as thermal radiation. Value range is [0.10~1.00].
                ('humidity', c_float),
                # The relative humidity of the environment. Value range is [20~100] percent. Defualt value is 70%.
                ('emissivity', c_float),
                # Reflected temperature in Celsius.
                # The surface of the target that is measured could reflect the energy radiated by the surrounding objects.
                # Value range is [-40.0~500.0]
                ('reflection', c_float),
            ]

        DIRP_SUCCESS = 0  # 0: Success (no error)
        DIRP_ERROR_MALLOC = -1  # -1: Malloc error
        DIRP_ERROR_POINTER_NULL = -2  # -2: NULL pointer input
        DIRP_ERROR_INVALID_PARAMS = -3  # -3: Invalid parameters input
        DIRP_ERROR_INVALID_RAW = -4  # -4: Invalid RAW in R-JPEG
        DIRP_ERROR_INVALID_HEADER = -5  # -5: Invalid header in R-JPEG
        DIRP_ERROR_INVALID_CURVE = -6  # -6: Invalid curve LUT in R-JPEG
        DIRP_ERROR_RJPEG_PARSE = -7  # -7: Parse error for R-JPEG data
        DIRP_ERROR_SIZE = -8  # -8: Wrong size input
        DIRP_ERROR_INVALID_HANDLE = -9  # -9: Invalid handle input
        DIRP_ERROR_FORMAT_INPUT = -10  # -10: Wrong input image format
        DIRP_ERROR_FORMAT_OUTPUT = -11  # -11: Wrong output image format
        DIRP_ERROR_UNSUPPORTED_FUNC = -12  # -12: Unsupported function called
        DIRP_ERROR_NOT_READY = -13  # -13: Some preliminary conditions not meet
        DIRP_ERROR_ACTIVATION = -14  # -14: SDK activate failed
        DIRP_ERROR_ADVANCED = -32  # -32: Advanced error codes which may be smaller than this value

        # Load dynamic link library
        _dll_dirp = CDLL(filepath_dll)

        _dirp_set_verbose_level = _dll_dirp.dirp_set_verbose_level
        _dirp_set_verbose_level.argtypes = [c_int]
        _dirp_set_verbose_level(DIRP_VERBOSE_LEVEL_NONE)

        _dirp_create_from_rjpeg = _dll_dirp.dirp_create_from_rjpeg
        _dirp_create_from_rjpeg.argtypes = [POINTER(c_uint8), c_int32, POINTER(DIRP_HANDLE)]
        _dirp_create_from_rjpeg.restype = c_int32

        # Destroy the DIRP handle.
        _dirp_destroy = _dll_dirp.dirp_destroy
        _dirp_destroy.argtypes = [DIRP_HANDLE]
        _dirp_destroy.restype = c_int32

        _dirp_get_rjpeg_version = _dll_dirp.dirp_get_rjpeg_version
        _dirp_get_rjpeg_version.argtypes = [DIRP_HANDLE, POINTER(dirp_rjpeg_version_t)]
        _dirp_get_rjpeg_version.restype = c_int32

        _dirp_get_rjpeg_resolution = _dll_dirp.dirp_get_rjpeg_resolution
        _dirp_get_rjpeg_resolution.argtypes = [DIRP_HANDLE, POINTER(dirp_resolution_t)]
        _dirp_get_rjpeg_resolution.restype = c_int32

        # Get orignial/custom temperature measurement parameters.
        _dirp_get_measurement_params = _dll_dirp.dirp_get_measurement_params
        _dirp_get_measurement_params.argtypes = [DIRP_HANDLE, POINTER(dirp_measurement_params_t)]
        _dirp_get_measurement_params.restype = c_int32

        # Set custom temperature measurement parameters.
        _dirp_set_measurement_params = _dll_dirp.dirp_set_measurement_params
        _dirp_set_measurement_params.argtypes = [DIRP_HANDLE, POINTER(dirp_measurement_params_t)]
        _dirp_set_measurement_params.restype = c_int32

        # Measure temperature of whole thermal image with RAW data in R-JPEG.
        # Each INT16 pixel value represents ten times the temperature value in Celsius. In other words,
        # each LSB represents 0.1 degrees Celsius.
        _dirp_measure = _dll_dirp.dirp_measure
        _dirp_measure.argtypes = [DIRP_HANDLE, POINTER(c_int16), c_int32]
        _dirp_measure.restype = c_int32

        # Measure temperature of whole thermal image with RAW data in R-JPEG.
        # Each float32 pixel value represents the real temperature in Celsius.
        _dirp_measure_ex = _dll_dirp.dirp_measure_ex
        _dirp_measure_ex.argtypes = [DIRP_HANDLE, POINTER(c_float), c_int32]
        _dirp_measure_ex.restype = c_int32

        with open(filepath_image, 'rb') as file:
            raw = file.read()
            raw_size = c_int32(len(raw))
            raw_c_uint8 = cast(raw, POINTER(c_uint8))

        handle = DIRP_HANDLE()
        rjpeg_version = dirp_rjpeg_version_t()
        rjpeg_resolution = dirp_resolution_t()

        return_status = _dirp_create_from_rjpeg(raw_c_uint8, raw_size, handle)

        assert return_status == DIRP_SUCCESS, f'dirp_create_from_rjpeg error {filepath_image}:{return_status}'
        assert _dirp_get_rjpeg_version(handle, rjpeg_version) == DIRP_SUCCESS
        assert _dirp_get_rjpeg_resolution(handle, rjpeg_resolution) == DIRP_SUCCESS

        params = dirp_measurement_params_t()
        params_point = pointer(params)
        return_status = _dirp_get_measurement_params(handle, params_point)
        assert return_status == DIRP_SUCCESS, f'dirp_get_measurement_params error {filepath_image}:{return_status}'

        if isinstance(object_distance, (float, int)):
            params.distance = object_distance
        if isinstance(relative_humidity, (float, int)):
            params.humidity = relative_humidity
        if isinstance(emissivity, (float, int)):
            params.emissivity = emissivity
        if isinstance(reflected_apparent_temperature, (float, int)):
            params.reflection = reflected_apparent_temperature

        return_status = _dirp_set_measurement_params(handle, params)
        assert return_status == DIRP_SUCCESS, f'dirp_set_measurement_params error {filepath_image}:{return_status}'

        if _dtype.__name__ == np.float32.__name__:
            data = np.zeros(image_width * image_height, dtype=np.float32)
            data_ptr = data.ctypes.data_as(POINTER(c_float))
            data_size = c_int32(image_width * image_height * sizeof(c_float))
            assert _dirp_measure_ex(handle, data_ptr, data_size) == DIRP_SUCCESS
            temp = np.reshape(data, (image_height, image_width))

        elif _dtype.__name__ == np.int16.__name__:
            data = np.zeros(image_width * image_height, dtype=np.int16)
            data_ptr = data.ctypes.data_as(POINTER(c_int16))
            data_size = c_int32(image_width * image_height * sizeof(c_int16))
            assert _dirp_measure(handle, data_ptr, data_size) == DIRP_SUCCESS
            temp = np.reshape(data, (image_height, image_width))/10
        else:
            raise ValueError
        assert _dirp_destroy(handle) == DIRP_SUCCESS
        
        return temp
