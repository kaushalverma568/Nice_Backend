package com.nice.service.impl;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.dto.StateDTO;
import com.nice.dto.StateResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.StateMapper;
import com.nice.model.City;
import com.nice.model.Country;
import com.nice.model.State;
import com.nice.repository.StateRepository;
import com.nice.service.CityService;
import com.nice.service.CountryService;
import com.nice.service.StateService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 22-Jun-2020
 */
@Service(value = "stateService")
@Transactional(rollbackFor = Throwable.class)
public class StateServiceImpl implements StateService {

	private static final Logger LOGGER = LoggerFactory.getLogger(StateServiceImpl.class);

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private StateRepository stateRepository;

	@Autowired
	private CountryService countryService;

	@Autowired
	private CityService cityService;

	@Autowired
	private StateMapper stateMapper;

	@Override
	public State addState(final StateDTO stateDTO) throws ValidationException, NotFoundException {
		Country country = countryService.getCountryDetails(stateDTO.getCountryId());
		State state = stateMapper.toEntity(stateDTO);
		state.setCountry(country);
		return stateRepository.save(state);

	}

	@Override
	public State updateState(final StateDTO stateDTO) throws NotFoundException, ValidationException {
		if (stateDTO.getId() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("state.id.not.null", null));
		}
		/**
		 * Whether state is exists or not
		 */
		getStateDetails(stateDTO.getId());
		Country country = countryService.getCountryDetails(stateDTO.getCountryId());
		State state = stateMapper.toEntity(stateDTO);
		state.setCountry(country);
		return stateRepository.save(state);
	}

	@Override
	public StateResponseDTO getState(final Long stateId) throws NotFoundException {
		return stateMapper.toDto(getStateDetails(stateId));
	}

	@Override
	public State getStateDetails(final Long stateId) throws NotFoundException {
		return stateRepository.findById(stateId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("state.not.found", new Object[] { stateId })));
	}

	@Override
	public boolean isStateExists(final StateDTO stateDTO) throws NotFoundException {
		Country country = countryService.getCountryDetails(stateDTO.getCountryId());

		if (stateDTO.getId() != null) {
			/**
			 * At the time of update is state with same name for same country exist or not except it's own ID
			 */
			return stateRepository.findByNameIgnoreCaseAndCountryAndIdNot(stateDTO.getName(), country, stateDTO.getId()).isPresent();
		} else {
			/**
			 * At the time of create is state with same name for same country exist or not
			 */
			return stateRepository.findByNameIgnoreCaseAndCountry(stateDTO.getName(), country).isPresent();
		}
	}

	@Override
	public List<State> getStateListBasedOnParams(final Integer startIndex, final Integer pageSize, final Boolean activeRecords, final Long countryId,
			final String searchKeyword) throws ValidationException {
		return stateRepository.getStateList(startIndex, pageSize, activeRecords, countryId, searchKeyword);
	}

	@Override
	public void changeStatus(final Long stateId, final Boolean active) throws NotFoundException, ValidationException {
		State existingState = getStateDetails(stateId);
		LOGGER.info("Existing state details {} ", existingState);
		if (active == null) {
			throw new ValidationException(messageByLocaleService.getMessage("active.not.null", null));
		} else if (existingState.getActive().equals(active)) {
			throw new ValidationException(messageByLocaleService.getMessage(Boolean.TRUE.equals(active) ? "state.active" : "state.deactive", null));
		} else {
			changeStatusOfDependantEntity(active, existingState);
			existingState.setActive(active);
			stateRepository.save(existingState);
		}
	}

	/**
	 * Deactivate city and customer address and validation while activate state & validate country while state is activate
	 *
	 * @param  stateId
	 * @param  active
	 * @param  userId
	 * @param  existingState
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	private void changeStatusOfDependantEntity(final Boolean active, final State existingState) throws ValidationException, NotFoundException {
		if (Boolean.FALSE.equals(active)) {
			List<City> cityList = cityService.getCityListBasedOnParams(null, null, true, existingState.getId(), null);
			for (City city : cityList) {
				LOGGER.info("Deactive city for id : {} , because of state deactive", city.getId());
				cityService.changeStatus(city.getId(), active);
			}
		} else {
			if (Boolean.FALSE.equals(existingState.getCountry().getActive())) {
				throw new ValidationException(messageByLocaleService.getMessage("country.activate.first", null));
			}
		}
	}

	@Override
	public Long getStateCountBasedOnParams(final Boolean activeRecords, final Long countryId, final String searchKeyword) {
		return stateRepository.getStateCountBasedonParams(activeRecords, countryId, searchKeyword);
	}

}
