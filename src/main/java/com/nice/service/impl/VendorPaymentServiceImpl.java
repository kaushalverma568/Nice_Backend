package com.nice.service.impl;

import javax.transaction.Transactional;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.nice.dto.VendorPaymentDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.model.VendorPayment;
import com.nice.repository.VendorPaymentRepository;
import com.nice.service.SubscriptionPlanService;
import com.nice.service.VendorPaymentService;
import com.nice.service.VendorService;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 04-Sep-2020
 */

@Service("vendorPaymentService")
@Transactional(rollbackOn = Throwable.class)
public class VendorPaymentServiceImpl implements VendorPaymentService {

	private static final Logger LOGGER = LoggerFactory.getLogger(VendorPaymentServiceImpl.class);

	@Autowired
	private VendorPaymentRepository vendorPaymentRepository;

	@Autowired
	private VendorService vendorService;

	@Autowired
	private SubscriptionPlanService subscriptionPlanService;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Override
	public void addVendorPayment(final VendorPaymentDTO vendorPaymentDTO) throws NotFoundException {
		LOGGER.info("Inside add vendor payment");
		VendorPayment vendorPayment = new VendorPayment();
		BeanUtils.copyProperties(vendorPaymentDTO, vendorPayment);
		vendorPayment.setVendor(vendorService.getVendorDetail(vendorPaymentDTO.getVendorId()));
		vendorPayment.setSubscriptionPlan(subscriptionPlanService.getSubscriptionPlanDetail(vendorPaymentDTO.getSubscriptionPlanId()));
		vendorPaymentRepository.save(vendorPayment);
		LOGGER.info("outside add vendor payment");
	}

	@Override
	public void updateVendorPayment(final VendorPayment vendorPayment) throws NotFoundException, ValidationException {
		LOGGER.info("Inside update vendor payment");
		validateVendorPayment(vendorPayment);
		vendorPaymentRepository.save(vendorPayment);
		LOGGER.info("Outside update vendor payment");
	}

	private void validateVendorPayment(final VendorPayment vendorPayment) throws ValidationException, NotFoundException {
		LOGGER.info("Inside validate vendor payment");
		getVendorPaymentByVendorOrderId(vendorPayment.getVendorOrderId());
		if (!CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(vendorPayment.getPaymentId())) {
			throw new ValidationException(messageByLocaleService.getMessage("payment.id.not.null", new Object[] { vendorPayment.getPaymentId() }));
		}
		if (!CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(vendorPayment.getPaymentToken())) {
			throw new ValidationException(messageByLocaleService.getMessage("payment.token.not.null", new Object[] { vendorPayment.getPaymentToken() }));
		}
		if (vendorPayment.getAdministrativeCharge() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("admin.charge.not.null", new Object[] { vendorPayment.getAdministrativeCharge() }));
		}
		LOGGER.info("Outside validate vendor payment");
	}

	@Override
	public VendorPayment getVendorPaymentByVendorOrderId(final String vendorOrderId) throws NotFoundException {
		return vendorPaymentRepository.findByVendorOrderId(vendorOrderId).orElseThrow(
				() -> new NotFoundException(messageByLocaleService.getMessage("vendor.payment.not.found.orderId", new Object[] { vendorOrderId })));
	}

	@Override
	public VendorPayment getVendorPaymentByVendorOrderIdAndStatus(final String vendorOrderId, final String status) throws NotFoundException {
		return vendorPaymentRepository.findByVendorOrderIdAndStatus(vendorOrderId, status).orElseThrow(() -> new NotFoundException(
				messageByLocaleService.getMessage("vendor.payment.not.found.orderId.status", new Object[] { vendorOrderId, status })));
	}

	@Override
	public VendorPaymentDTO getVendorPaymentDTOByVendorOrderId(final String vendorOrderId) throws NotFoundException {
		VendorPayment vendorPayment = vendorPaymentRepository.findByVendorOrderId(vendorOrderId).orElseThrow(
				() -> new NotFoundException(messageByLocaleService.getMessage("vendor.payment.not.found.orderId", new Object[] { vendorOrderId })));
		VendorPaymentDTO vendorPaymentDTO = new VendorPaymentDTO();
		BeanUtils.copyProperties(vendorPayment, vendorPaymentDTO);
		return vendorPaymentDTO;
	}

	@Override
	public VendorPayment getVendorPaymentById(final Long id) throws NotFoundException {
		return vendorPaymentRepository.findById(id)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("vendor.payment.not.found", new Object[] { id })));
	}

}
