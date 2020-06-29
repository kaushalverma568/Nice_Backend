package com.nice.service.impl;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.dto.PincodeDTO;
import com.nice.dto.PincodeResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.PincodeMapper;
import com.nice.model.City;
import com.nice.model.Pincode;
import com.nice.repository.PincodeRepository;
import com.nice.service.CityService;
import com.nice.service.CustomerAddressService;
import com.nice.service.PincodeService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 23-Jun-2020
 */
@Service("pincodeService")
@Transactional(rollbackFor = Throwable.class)
public class PincodeServiceImpl implements PincodeService {

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	private static final Logger LOGGER = LoggerFactory.getLogger(PincodeServiceImpl.class);

	@Autowired
	private PincodeRepository pincodeRepository;

	@Autowired
	private CityService cityService;

	@Autowired
	private PincodeMapper pincodeMapper;

	@Autowired
	private CustomerAddressService customerAddressService;

	@Override
	public void addPincode(final PincodeDTO pincodeDTO) throws ValidationException, NotFoundException {
		final City city = cityService.getCityDetails(pincodeDTO.getCityId());
		final Pincode pincode = pincodeMapper.toEntity(pincodeDTO);
		pincode.setCity(city);
		pincodeRepository.save(pincode);
	}

	@Override
	public PincodeResponseDTO getPincode(final Long pincodeId) throws NotFoundException {
		return pincodeMapper.toDto(getPincodeDetails(pincodeId));
	}

	@Override
	public Pincode getPincodeDetails(final Long pincodeId) throws NotFoundException {
		return pincodeRepository.findById(pincodeId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("pincode.not.found", new Object[] { pincodeId })));
	}

	@Override
	public Boolean isPincodeExists(final PincodeDTO pincodeDTO) {
		if (pincodeDTO.getId() != null) {
			/**
			 * At the time of update is pincode with same codeValue exist or not except it's own ID
			 */
			return pincodeRepository.findByCodeValueIgnoreCaseAndIdNot(pincodeDTO.getCodeValue(), pincodeDTO.getId()).isPresent();
		} else {
			/**
			 * At the time of add is pincode with same codeValue exist or not
			 */
			return pincodeRepository.findByCodeValueIgnoreCase(pincodeDTO.getCodeValue()).isPresent();
		}
	}

	@Override
	public void changeStatus(final Long pincodeId, final Boolean active) throws ValidationException, NotFoundException {
		final Pincode existingPincode = getPincodeDetails(pincodeId);
		LOGGER.info("Existing pincode details {} ", existingPincode);
		if (active == null) {
			throw new ValidationException(messageByLocaleService.getMessage("active.not.null", null));
		} else if (existingPincode.getActive().equals(active)) {
			throw new ValidationException(messageByLocaleService.getMessage(Boolean.TRUE.equals(active) ? "pincode.active" : "pincode.deactive", null));
		} else {
			changeStatusOfDependantEntity(existingPincode, active);
			existingPincode.setActive(active);
			pincodeRepository.save(existingPincode);
		}
	}

	/**
	 * Deactivate customer address and validation while activate pincode & validate city while pincode is activate
	 *
	 * @param  pincodeId
	 * @param  active
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	private void changeStatusOfDependantEntity(final Pincode existingPincode, final Boolean active) throws ValidationException {
		if (Boolean.FALSE.equals(active)) {
			/**
			 * Delete all the customer address for the pincode
			 */
			customerAddressService.deleteAllAddressByPincode(existingPincode);

		} else {
			if (Boolean.FALSE.equals(existingPincode.getCity().getActive())) {
				throw new ValidationException(messageByLocaleService.getMessage("city.activate.first", null));
			}
		}
	}

	@Override
	public Long getPincodeCountBasedOnParams(final Boolean activeRecords, final Long cityId, final String searchKeyword) {
		return pincodeRepository.getPincodeCountBasedOnParams(activeRecords, cityId, searchKeyword);
	}

	@Override
	public List<Pincode> getPincodeListBasedOnParams(final Integer startIndex, final Integer pageSize, final Boolean activeRecords, final Long cityId,
			final String searchKeyword) {
		return pincodeRepository.getPincodeListBasedOnParams(startIndex, pageSize, activeRecords, cityId, searchKeyword);
	}

}
