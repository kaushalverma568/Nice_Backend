package com.nice.service.impl;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.dto.CityDTO;
import com.nice.dto.CityResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.CityMapper;
import com.nice.model.City;
import com.nice.model.Pincode;
import com.nice.model.State;
import com.nice.repository.CityRepository;
import com.nice.service.CityService;
import com.nice.service.PincodeService;
import com.nice.service.StateService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 22-Jun-2020
 */
@Service(value = "cityService")
@Transactional(rollbackFor = Throwable.class)
public class CityServiceImpl implements CityService {

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	private static final Logger LOGGER = LoggerFactory.getLogger(CityServiceImpl.class);

	@Autowired
	private CityRepository cityRepository;

	@Autowired
	private StateService stateService;

	@Autowired
	private CityMapper cityMapper;

	@Autowired
	private PincodeService pincodeService;

	@Override
	public void addCity(final CityDTO cityDTO) throws ValidationException, NotFoundException {
		State state = stateService.getStateDetails(cityDTO.getStateId());
		City city = cityMapper.toEntity(cityDTO);
		city.setState(state);
		cityRepository.save(city);
	}

	@Override
	public void updateCity(final CityDTO cityDTO) throws ValidationException, NotFoundException {
		if (cityDTO.getId() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("city.id.not.null", null));
		}
		/**
		 * Check whether city is exists or not.
		 */
		getCityDetails(cityDTO.getId());
		State state = stateService.getStateDetails(cityDTO.getStateId());
		City city = cityMapper.toEntity(cityDTO);
		city.setState(state);
		cityRepository.save(city);
	}

	@Override
	public CityResponseDTO getCity(final Long cityId) throws NotFoundException {
		return cityMapper.toDto(getCityDetails(cityId));
	}

	@Override
	public City getCityDetails(final Long cityId) throws NotFoundException {
		return cityRepository.findById(cityId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("city.not.found", new Object[] { cityId })));
	}

	@Override
	public boolean isCityExistsEnglish(final CityDTO cityDTO) throws NotFoundException {
		State state = stateService.getStateDetails(cityDTO.getStateId());
		if (cityDTO.getId() != null) {
			/**
			 * At the time of update is city with same english name or arabic name for same state exist or not except it's own ID
			 */
			return cityRepository.findByNameEnglishIgnoreCaseAndStateAndIdNot(cityDTO.getNameEnglish(), state, cityDTO.getId()).isPresent();
		} else {
			/**
			 * At the time of create is city with same english name or arabic name for same state exist or not
			 */
			return cityRepository.findByNameEnglishIgnoreCaseAndState(cityDTO.getNameEnglish(), state).isPresent();
		}
	}

	@Override
	public boolean isCityExistsArabic(final CityDTO cityDTO) throws NotFoundException {
		State state = stateService.getStateDetails(cityDTO.getStateId());
		if (cityDTO.getId() != null) {
			/**
			 * At the time of update is city with same english name or arabic name for same state exist or not except it's own ID
			 */
			return cityRepository.findByNameArabicIgnoreCaseAndStateAndIdNot(cityDTO.getNameArabic(), state, cityDTO.getId()).isPresent();
		} else {
			/**
			 * At the time of create is city with same english name or arabic name for same state exist or not
			 */
			return cityRepository.findByNameArabicIgnoreCaseAndState(cityDTO.getNameArabic(), state).isPresent();
		}
	}

	@Override
	public void changeStatus(final Long cityId, final Boolean active) throws ValidationException, NotFoundException {
		City existingCity = getCityDetails(cityId);
		LOGGER.info("Existing city details {} ", existingCity);
		if (active == null) {
			throw new ValidationException(messageByLocaleService.getMessage("active.not.null", null));
		} else if (existingCity.getActive().equals(active)) {
			throw new ValidationException(messageByLocaleService.getMessage(Boolean.TRUE.equals(active) ? "city.active" : "city.deactive", null));
		} else {
			changeStatusOfDependantEntity(existingCity, active);

			existingCity.setActive(active);
			cityRepository.save(existingCity);
		}

	}

	/**
	 * Deactivate customer address and validation while activate city & validate state while city is activate
	 *
	 * @param  cityId
	 * @param  active
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	private void changeStatusOfDependantEntity(final City existingCity, final Boolean active) throws ValidationException, NotFoundException {
		if (Boolean.FALSE.equals(active)) {
			final List<Pincode> pincodeList = pincodeService.getPincodeListBasedOnParams(null, null, true, existingCity.getId(), null);
			for (Pincode pincode : pincodeList) {
				LOGGER.info("Deactive pincode for id : {} , because of city deactive", pincode.getId());
				pincodeService.changeStatus(pincode.getId(), active);
			}
		} else {
			if (Boolean.FALSE.equals(existingCity.getState().getActive())) {
				throw new ValidationException(messageByLocaleService.getMessage("state.activate.first", null));
			}
		}
	}

	@Override
	public Long getCityCountBasedOnParams(final Boolean activeRecords, final Long stateId, final String searchKeyword, final Boolean isPincodeExist) {
		Set<Long> idsIn = new HashSet<>();
		/**
		 * For getting only those cities which has active pin codes
		 */
		if (isPincodeExist != null && isPincodeExist) {
			final List<Pincode> pincodeList = pincodeService.getPincodeListBasedOnParams(null, null, true, null, null);
			for (Pincode pincode : pincodeList) {
				idsIn.add(pincode.getCity().getId());
			}
		}
		return cityRepository.getCityCountBasedOnParams(activeRecords, stateId, searchKeyword, idsIn);
	}

	@Override
	public List<City> getCityListBasedOnParams(final Integer startIndex, final Integer pageSize, final Boolean activeRecords, final Long stateId,
			final String searchKeyword, final Boolean isPincodeExist) {
		Set<Long> idsIn = new HashSet<>();
		/**
		 * For getting only those cities which has active pin codes
		 */
		if (isPincodeExist != null && isPincodeExist) {
			final List<Pincode> pincodeList = pincodeService.getPincodeListBasedOnParams(null, null, true, null, null);
			for (Pincode pincode : pincodeList) {
				idsIn.add(pincode.getCity().getId());
			}
		}
		return cityRepository.getCityListBasedOnParams(startIndex, pageSize, activeRecords, stateId, searchKeyword, idsIn);
	}

}
