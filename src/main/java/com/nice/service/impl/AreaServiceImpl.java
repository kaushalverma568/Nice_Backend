package com.nice.service.impl;

import java.util.List;
import java.util.Locale;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.dto.AreaDTO;
import com.nice.dto.AreaResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.AreaMapper;
import com.nice.model.Area;
import com.nice.model.City;
import com.nice.repository.AreaRepository;
import com.nice.service.AreaService;
import com.nice.service.CityService;
import com.nice.service.CustomerAddressService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : Oct 9, 2020
 */
@Service(value = "areaService")
@Transactional(rollbackFor = Throwable.class)
public class AreaServiceImpl implements AreaService {

	private static final Logger LOGGER = LoggerFactory.getLogger(AreaServiceImpl.class);

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private AreaRepository areaRepository;

	@Autowired
	private AreaMapper areaMapper;

	@Autowired
	private CustomerAddressService customerAddressService;

	@Autowired
	private CityService cityService;

	@Override
	public void addArea(final AreaDTO areaDTO) throws ValidationException, NotFoundException {
		Area area = areaMapper.toEntity(areaDTO);
		City city = cityService.getCityDetails(areaDTO.getCityId());
		area.setCity(city);
		areaRepository.save(area);
	}

	@Override
	public void updateArea(final AreaDTO areaDTO) throws NotFoundException, ValidationException {
		if (areaDTO.getId() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("area.id.not.null", null));
		}
		Area area = areaMapper.toEntity(areaDTO);
		City city = cityService.getCityDetails(areaDTO.getCityId());
		area.setCity(city);
		areaRepository.save(area);
	}

	@Override
	public AreaResponseDTO getArea(final Long areaId) throws NotFoundException {
		return areaMapper.toDto(getAreaDetails(areaId));
	}

	@Override
	public Area getAreaDetails(final Long areaId) throws NotFoundException {
		return areaRepository.findById(areaId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("area.not.found", new Object[] { areaId })));
	}

	@Override
	public Page<Area> getAreaList(final Integer pageNumber, final Integer pageSize, final Boolean activeRecords, final String searchKeyWord, final Long cityId)
			throws NotFoundException, ValidationException {
		Pageable pageable;
		Locale locale = LocaleContextHolder.getLocale();
		if (locale.getLanguage().equals("en")) {
			pageable = PageRequest.of(pageNumber - 1, pageSize, Sort.by("nameEnglish"));
		} else {
			pageable = PageRequest.of(pageNumber - 1, pageSize, Sort.by("nameArabic"));
		}
		if (cityId != null) {
			City city = cityService.getCityDetails(cityId);
			if (activeRecords != null) {
				if (searchKeyWord != null) {
					return areaRepository.findAllByActiveAndCityAndNameEnglishContainingIgnoreCaseOrActiveAndCityAndNameArabicContainingIgnoreCase(
							activeRecords, city, searchKeyWord, activeRecords, city, searchKeyWord, pageable);
				} else {
					return areaRepository.findAllByActiveAndCity(activeRecords, city, pageable);
				}
			} else {
				if (searchKeyWord != null) {
					return areaRepository.findAllByCityAndNameEnglishContainingIgnoreCaseOrCityAndNameArabicContainingIgnoreCase(city, searchKeyWord, city,
							searchKeyWord, pageable);
				} else {
					return areaRepository.findAllByCity(city, pageable);
				}
			}
		} else {
			return getAreaListByActiveAndSearchKeyword(activeRecords, searchKeyWord, pageable);
		}
	}

	private Page<Area> getAreaListByActiveAndSearchKeyword(final Boolean activeRecords, final String searchKeyWord, final Pageable pageable) {
		if (activeRecords != null) {
			if (searchKeyWord != null) {
				return areaRepository.findAllByActiveAndNameEnglishContainingIgnoreCaseOrActiveAndNameArabicContainingIgnoreCase(activeRecords, searchKeyWord,
						activeRecords, searchKeyWord, pageable);
			} else {
				return areaRepository.findAllByActive(activeRecords, pageable);
			}
		} else {
			if (searchKeyWord != null) {
				return areaRepository.findAllByNameEnglishContainingIgnoreCaseOrNameArabicContainingIgnoreCase(searchKeyWord, searchKeyWord, pageable);
			} else {
				return areaRepository.findAll(pageable);
			}
		}
	}

	@Override
	public void changeStatus(final Long areaId, final Boolean active) throws NotFoundException, ValidationException {
		Area existingArea = getAreaDetails(areaId);
		LOGGER.info("Existing area details {} ", existingArea);
		if (active == null) {
			throw new ValidationException(messageByLocaleService.getMessage("active.not.null", null));
		} else if (existingArea.getActive().equals(active)) {
			throw new ValidationException(messageByLocaleService.getMessage(Boolean.TRUE.equals(active) ? "area.active" : "area.deactive", null));
		} else {
			changeStatusOfDependantEntity(existingArea, active);
			existingArea.setActive(active);
			areaRepository.save(existingArea);
		}
	}

	/**
	 * Deactivate customer address while deactivating area
	 *
	 * @param  existingArea
	 * @param  active
	 * @throws ValidationException
	 */
	private void changeStatusOfDependantEntity(final Area existingArea, final Boolean active) {
		if (Boolean.FALSE.equals(active)) {
			/**
			 * Delete all the customer address for the area
			 */
			customerAddressService.deleteAllAddressByArea(existingArea);
		}
	}

	@Override
	public boolean isAreaExistsEnglish(final AreaDTO areaDTO) throws NotFoundException {
		City city = cityService.getCityDetails(areaDTO.getCityId());
		if (areaDTO.getId() != null) {
			/**
			 * At the time of update is area with same English exist or not except it's own id
			 */
			return areaRepository.findByNameEnglishIgnoreCaseAndCityAndIdNot(areaDTO.getNameEnglish(), city, areaDTO.getId()).isPresent();
		} else {
			/**
			 * At the time of create is area with same english or arabic name exist or not
			 */
			return areaRepository.findByNameEnglishIgnoreCaseAndCity(areaDTO.getNameEnglish(), city).isPresent();
		}
	}

	@Override
	public boolean isAreaExistsArabic(final AreaDTO areaDTO) throws NotFoundException {
		City city = cityService.getCityDetails(areaDTO.getCityId());
		if (areaDTO.getId() != null) {
			/**
			 * At the time of update is area with same arabic name exist or not except it's own id
			 */
			return areaRepository.findByNameArabicIgnoreCaseAndCityAndIdNot(areaDTO.getNameArabic(), city, areaDTO.getId()).isPresent();
		} else {
			/**
			 * At the time of create is area with same arabic name exist or not
			 */
			return areaRepository.findByNameArabicIgnoreCaseAndCity(areaDTO.getNameArabic(), city).isPresent();
		}
	}

	@Override
	public List<Area> getAreaList(final Boolean activeRecords, final Long cityId) throws NotFoundException {
		if (cityId != null) {
			City city = cityService.getCityDetails(cityId);
			if (activeRecords != null) {
				return areaRepository.findAllByActiveAndCity(activeRecords, city);
			} else {
				return areaRepository.findAllByCity(city);
			}
		} else {
			if (activeRecords != null) {
				return areaRepository.findAllByActive(activeRecords);
			} else {
				return areaRepository.findAll();
			}
		}
	}
}
