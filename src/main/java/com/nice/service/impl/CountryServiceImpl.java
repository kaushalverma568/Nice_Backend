package com.nice.service.impl;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.dto.CountryDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.CountryMapper;
import com.nice.model.Country;
import com.nice.model.State;
import com.nice.repository.CountryRepository;
import com.nice.service.CountryService;
import com.nice.service.StateService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 22-Jun-2020
 */
@Service(value = "countryService")
@Transactional(rollbackFor = Throwable.class)
public class CountryServiceImpl implements CountryService {

	private static final Logger LOGGER = LoggerFactory.getLogger(CountryServiceImpl.class);

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private CountryRepository countryRepository;

	@Autowired
	private CountryMapper countryMapper;

	@Autowired
	private StateService stateService;

	@Override
	public Country addCountry(final CountryDTO countryDTO) throws ValidationException {
		return countryRepository.save(countryMapper.toEntity(countryDTO));
	}

	@Override
	public Country updateCountry(final CountryDTO countryDTO) throws NotFoundException, ValidationException {
		if (countryDTO.getId() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("country.id.not.null", null));
		}
		return countryRepository.save(countryMapper.toEntity(countryDTO));
	}

	@Override
	public CountryDTO getCountry(final Long countryId) throws NotFoundException {
		return countryMapper.toDto(getCountryDetails(countryId));
	}

	@Override
	public Country getCountryDetails(final Long countryId) throws NotFoundException {
		return countryRepository.findById(countryId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("country.not.found", new Object[] { countryId })));
	}

	@Override
	public Page<Country> getCountryList(final Integer pageNumber, final Integer pageSize, final Boolean activeRecords, final String searchKeyWord)
			throws NotFoundException, ValidationException {
		Pageable pageable = PageRequest.of(pageNumber - 1, pageSize, Sort.by("name"));
		if (activeRecords != null) {
			if (searchKeyWord != null) {
				return countryRepository.findAllByActiveAndNameContainingIgnoreCase(activeRecords, searchKeyWord, pageable);
			} else {
				return countryRepository.findAllByActive(activeRecords, pageable);
			}
		} else {
			if (searchKeyWord != null) {
				return countryRepository.findAllByNameContainingIgnoreCase(searchKeyWord, pageable);
			} else {
				return countryRepository.findAll(pageable);
			}
		}
	}

	@Override
	public void changeStatus(final Long countryId, final Boolean active) throws NotFoundException, ValidationException {
		Country existingCountry = getCountryDetails(countryId);
		LOGGER.info("Existing country details {} ", existingCountry);
		if (active == null) {
			throw new ValidationException(messageByLocaleService.getMessage("active.not.null", null));
		} else if (existingCountry.getActive().equals(active)) {
			throw new ValidationException(messageByLocaleService.getMessage(Boolean.TRUE.equals(active) ? "country.active" : "country.deactive", null));
		} else {
			changeStatusOfDependantEntity(existingCountry, active);
			existingCountry.setActive(active);
			countryRepository.save(existingCountry);
		}
	}

	/**
	 * @param  countryId
	 * @param  active
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	private void changeStatusOfDependantEntity(final Country country, final Boolean active) throws ValidationException, NotFoundException {
		if (Boolean.FALSE.equals(active)) {
			List<State> stateList = stateService.getStateListBasedOnParams(null, null, true, country.getId(), null);
			for (State state : stateList) {
				LOGGER.info("Deactive state for id : {} , because of country deactive", state.getId());
				stateService.changeStatus(state.getId(), active);
			}
		}
	}

	@Override
	public boolean isCountryExists(final CountryDTO countryDTO) {

		if (countryDTO.getId() != null) {
			/**
			 * At the time of update is country with same name exist or not except its own id.
			 */
			return countryRepository.findByNameIgnoreCaseAndIdNot(countryDTO.getName(), countryDTO.getId()).isPresent();
		} else {
			/**
			 * findByNameIgnoreCase At the time of create is country with same name exist or not
			 */
			return countryRepository.findByNameIgnoreCase(countryDTO.getName()).isPresent();
		}
	}

	@Override
	public Long getCountOfCountry() {
		return countryRepository.count();
	}

}
