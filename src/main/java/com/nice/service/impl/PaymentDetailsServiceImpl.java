package com.nice.service.impl;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import javax.servlet.http.HttpServletResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.config.UserAwareUserDetails;
import com.nice.constant.NotificationQueueConstants;
import com.nice.constant.UserType;
import com.nice.dto.DeliveryBoyPayoutDTO;
import com.nice.dto.Notification;
import com.nice.dto.PayableAmountDTO;
import com.nice.dto.PaymentDetailsDTO;
import com.nice.dto.PaymentDetailsResponseDTO;
import com.nice.dto.PushNotificationDTO;
import com.nice.dto.VendorPayoutDTO;
import com.nice.exception.FileOperationException;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.jms.queue.JMSQueuerService;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.PaymentDetailsMapper;
import com.nice.model.DeliveryBoy;
import com.nice.model.PaymentDetails;
import com.nice.model.Task;
import com.nice.model.UserLogin;
import com.nice.model.Vendor;
import com.nice.repository.PaymentDetailsRepository;
import com.nice.repository.TaskRepository;
import com.nice.service.DeliveryBoyService;
import com.nice.service.PaymentDetailsService;
import com.nice.service.TaskService;
import com.nice.service.VendorService;
import com.nice.util.CommonUtility;
import com.nice.util.ExportCSV;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 20-Jul-2020
 */
@Transactional(rollbackFor = Throwable.class)
@Service("paymentDetailsService")
public class PaymentDetailsServiceImpl implements PaymentDetailsService {

	private static final String PAYMENT_ALREADY_DONE_ORDER = "payment.already.done.order";

	private static final String EXPORT_FILE_CREATE_ERROR = "export.file.create.error";

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private PaymentDetailsRepository paymentDetailsRepository;

	@Autowired
	private PaymentDetailsMapper paymentDetailsMapper;

	@Autowired
	private TaskService taskService;

	@Autowired
	private TaskRepository taskRepository;

	@Autowired
	private DeliveryBoyService deliveryBoyService;

	@Autowired
	private VendorService vendorService;

	@Autowired
	private ExportCSV exportCSV;

	@Autowired
	private JMSQueuerService jmsQueuerService;

	@Override
	public Long addPaymentDetails(final PaymentDetailsDTO paymentDetailsDTO) throws NotFoundException, ValidationException {
		List<Task> taskList = new ArrayList<>();

		for (Long taskId : paymentDetailsDTO.getTaskIds()) {
			taskList.add(taskService.getTaskDetail(taskId));
		}
		PaymentDetails paymentDetails = paymentDetailsMapper.toEntity(paymentDetailsDTO);
		paymentDetails.setActive(true);
		/**
		 * if tasks do not have unique delivery boy OR vendor then throw exception
		 */
		if (UserType.DELIVERY_BOY.name().equals(paymentDetailsDTO.getEntityType())) {
			List<DeliveryBoy> deliveryBoys = taskList.stream().map(Task::getDeliveryBoy).distinct().collect(Collectors.toList());
			if (!deliveryBoys.isEmpty() && deliveryBoys.size() > 1) {
				throw new ValidationException(messageByLocaleService.getMessage("orders.belong.multiple.delivery.boy", null));
			}
			DeliveryBoy deliveryBoy = deliveryBoys.get(0);
			if (!CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(deliveryBoy.getBankAccountNumber())) {
				throw new ValidationException(messageByLocaleService.getMessage("deliveryboy.bank.detail.required", null));
			}
			/**
			 * get task list calculate amount and validate it
			 */
			Double sum = 0d;
			for (Task task : taskList) {
				if (task.getDeliveryBoyPaymentDetails() != null) {
					throw new ValidationException(messageByLocaleService.getMessage(PAYMENT_ALREADY_DONE_ORDER, new Object[] { task.getOrder().getId() }));
				}
				sum += task.getDeliveryCharge();
			}
			if (!sum.equals(paymentDetailsDTO.getPaymentAmount())) {
				throw new ValidationException(messageByLocaleService.getMessage("invalid.payment.amount", new Object[] { sum }));
			}
			paymentDetails.setDeliveryBoy(deliveryBoys.get(0));
		} else if (UserType.VENDOR.name().equals(paymentDetailsDTO.getEntityType())) {
			List<Vendor> vendors = taskList.stream().map(Task::getVendor).distinct().collect(Collectors.toList());
			if (!vendors.isEmpty() && vendors.size() > 1) {
				throw new ValidationException(messageByLocaleService.getMessage("orders.belong.multiple.vendor", null));
			}
			/**
			 * get task list calculate amount and validate it
			 */
			Double sum = 0d;
			for (Task task : taskList) {
				if (task.getVendorPaymentDetails() != null) {
					throw new ValidationException(messageByLocaleService.getMessage(PAYMENT_ALREADY_DONE_ORDER, new Object[] { task.getOrder().getId() }));
				}
				sum += task.getVendorPayableAmt();
			}
			if (!sum.equals(paymentDetailsDTO.getPaymentAmount())) {
				throw new ValidationException(messageByLocaleService.getMessage("invalid.payment.amount", new Object[] { sum }));
			}
			paymentDetails.setVendor(vendors.get(0));
		} else {
			throw new ValidationException(messageByLocaleService.getMessage("invalid.user.type", null));
		}
		/**
		 * Setting total no of orders based on task list here
		 */
		paymentDetails.setNoOfOrders(taskList.size());
		paymentDetails = paymentDetailsRepository.save(paymentDetails);

		/**
		 * Saving the payment details in task of delivery boy and vendor
		 */
		if (UserType.DELIVERY_BOY.name().equals(paymentDetailsDTO.getEntityType())) {
			for (Task task : taskList) {
				task.setDeliveryBoyPaymentDetails(paymentDetails);
			}
		} else {
			for (Task task : taskList) {
				task.setVendorPaymentDetails(paymentDetails);
			}
		}
		taskRepository.saveAll(taskList);
		return paymentDetails.getId();
	}

	@Override
	public PaymentDetailsResponseDTO getPaymentDetails(final Long paymentDetailsId) throws NotFoundException {
		final PaymentDetails existingPaymentDetails = getPaymentDetailsDetail(paymentDetailsId);
		return paymentDetailsMapper.toDto(existingPaymentDetails);
	}

	@Override
	public PaymentDetails getPaymentDetailsDetail(final Long paymentDetailsId) throws NotFoundException {
		return paymentDetailsRepository.findById(paymentDetailsId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("payment.details.not.found", new Object[] { paymentDetailsId })));
	}

	@Override
	public Boolean isPaymentDetailsExists(final PaymentDetailsDTO paymentDetailsDTO) {
		if (paymentDetailsDTO.getId() != null) {
			/**
			 * At the time of update is paymentDetails with same transaction no exist or not except it's own id
			 */
			return paymentDetailsRepository.findByTransactionNoIgnoreCaseAndIdNot(paymentDetailsDTO.getTransactionNo(), paymentDetailsDTO.getId()).isPresent();
		} else {
			/**
			 * At the time of create is paymentDetails with same transaction no exist or not
			 */
			return paymentDetailsRepository.findByTransactionNoIgnoreCase(paymentDetailsDTO.getTransactionNo()).isPresent();
		}
	}

	@Override
	public Page<PaymentDetails> getPaymentHistory(final Long deliveryBoyId, Long vendorId, final Date fromDate, final Date toDate, final Integer pageNumber,
			final Integer pageSize) throws ValidationException, NotFoundException {
		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(userLogin.getEntityType()) && UserType.VENDOR.name().equals(userLogin.getEntityType())) {
			vendorId = userLogin.getEntityId();
		}
		if ((deliveryBoyId == null && vendorId == null) || (deliveryBoyId != null && vendorId != null)) {
			throw new ValidationException(messageByLocaleService.getMessage("deliveryboy.or.vendor.required", null));
		}
		Pageable pageable = PageRequest.of(pageNumber - 1, pageSize, Sort.by(Direction.DESC, "id"));
		if (deliveryBoyId != null) {
			DeliveryBoy deliveryBoy = deliveryBoyService.getDeliveryBoyDetail(deliveryBoyId);
			if (fromDate != null) {
				if (toDate != null) {
					return paymentDetailsRepository.findAllByPaidOnBetweenAndDeliveryBoy(fromDate, toDate, deliveryBoy, pageable);
				} else {
					return paymentDetailsRepository.findAllByPaidOnGreaterThanEqualAndDeliveryBoy(fromDate, deliveryBoy, pageable);
				}
			} else {
				if (toDate != null) {
					return paymentDetailsRepository.findAllByPaidOnLessThanEqualAndDeliveryBoy(toDate, deliveryBoy, pageable);
				} else {
					return paymentDetailsRepository.findAllByDeliveryBoy(deliveryBoy, pageable);
				}
			}
		} else {
			Vendor vendor = vendorService.getVendorDetail(vendorId);
			if (fromDate != null) {
				if (toDate != null) {
					return paymentDetailsRepository.findAllByPaidOnBetweenAndVendor(fromDate, toDate, vendor, pageable);
				} else {
					return paymentDetailsRepository.findAllByPaidOnGreaterThanEqualAndVendor(fromDate, vendor, pageable);
				}
			} else {
				if (toDate != null) {
					return paymentDetailsRepository.findAllByPaidOnLessThanEqualAndVendor(toDate, vendor, pageable);
				} else {
					return paymentDetailsRepository.findAllByVendor(vendor, pageable);
				}
			}
		}
	}

	@Override
	public List<DeliveryBoyPayoutDTO> getDeliveryBoyPayout(final Long deliveryBoyId, final Date registeredOn, final Integer startIndex,
			final Integer pageSize) {
		return paymentDetailsRepository.getDeliveryBoyPayout(deliveryBoyId, registeredOn, startIndex, pageSize);
	}

	@Override
	public Long getDeliveryBoyPayoutCountBasedOnParam(final Long deliveryBoyId, final Date registeredOn) {
		return paymentDetailsRepository.getDeliveryBoyPayoutCountBasedOnParam(deliveryBoyId, registeredOn);
	}

	@Override
	public Long getVendorPayoutCountBasedOnParam(Long vendorId, final Long businessCategoryId) {
		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(userLogin.getEntityType()) && UserType.VENDOR.name().equals(userLogin.getEntityType())) {
			vendorId = userLogin.getEntityId();
		}
		return paymentDetailsRepository.getVendorPayoutCountBasedOnParam(vendorId, businessCategoryId);
	}

	@Override
	public List<VendorPayoutDTO> getVendorPayout(Long vendorId, final Long businessCategoryId, final Integer startIndex, final Integer pageSize) {
		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(userLogin.getEntityType()) && UserType.VENDOR.name().equals(userLogin.getEntityType())) {
			vendorId = userLogin.getEntityId();
		}
		return paymentDetailsRepository.getVendorPayout(vendorId, businessCategoryId, startIndex, pageSize);
	}

	@Override
	public Double getPayableAmountForTaskList(final PayableAmountDTO payableAmountDTO) throws NotFoundException, ValidationException {
		List<Task> taskList = new ArrayList<>();
		/**
		 * get task list calculate amount and validate it
		 */
		Double sum = 0d;

		for (Long taskId : payableAmountDTO.getTaskIds()) {
			taskList.add(taskService.getTaskDetail(taskId));
		}
		/**
		 * if tasks do not have unique delivery boy OR vendor then throw exception
		 */
		if (UserType.DELIVERY_BOY.name().equals(payableAmountDTO.getEntityType())) {
			List<DeliveryBoy> deliveryBoys = taskList.stream().map(Task::getDeliveryBoy).distinct().collect(Collectors.toList());
			if (!deliveryBoys.isEmpty() && deliveryBoys.size() > 1) {
				throw new ValidationException(messageByLocaleService.getMessage("orders.belong.multiple.delivery.boy", null));
			}
			for (Task task : taskList) {
				if (task.getDeliveryBoyPaymentDetails() != null) {
					throw new ValidationException(messageByLocaleService.getMessage(PAYMENT_ALREADY_DONE_ORDER, new Object[] { task.getOrder().getId() }));
				}
				sum += task.getDeliveryCharge();
			}
		} else if (UserType.VENDOR.name().equals(payableAmountDTO.getEntityType())) {
			List<Vendor> vendors = taskList.stream().map(Task::getVendor).distinct().collect(Collectors.toList());
			if (!vendors.isEmpty() && vendors.size() > 1) {
				throw new ValidationException(messageByLocaleService.getMessage("orders.belong.multiple.vendor", null));
			}
			for (Task task : taskList) {
				if (task.getVendorPaymentDetails() != null) {
					throw new ValidationException(messageByLocaleService.getMessage(PAYMENT_ALREADY_DONE_ORDER, new Object[] { task.getOrder().getId() }));
				}
				sum += task.getVendorPayableAmt();
			}
		} else {
			throw new ValidationException(messageByLocaleService.getMessage("invalid.user.type", null));
		}
		return sum;
	}

	@Override
	public void exportPaymentHistory(final HttpServletResponse httpServletResponse, final Long deliveryBoyId, Long vendorId, final Date fromDate,
			final Date toDate) throws NotFoundException, ValidationException, FileOperationException {
		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(userLogin.getEntityType()) && UserType.VENDOR.name().equals(userLogin.getEntityType())) {
			vendorId = userLogin.getEntityId();
		}
		if ((deliveryBoyId == null && vendorId == null) || (deliveryBoyId != null && vendorId != null)) {
			throw new ValidationException(messageByLocaleService.getMessage("deliveryboy.or.vendor.required", null));
		}
		List<PaymentDetails> paymentDetailsList = new ArrayList<>();
		if (deliveryBoyId != null) {
			DeliveryBoy deliveryBoy = deliveryBoyService.getDeliveryBoyDetail(deliveryBoyId);
			if (fromDate != null) {
				if (toDate != null) {
					paymentDetailsList = paymentDetailsRepository.findAllByPaidOnBetweenAndDeliveryBoy(fromDate, toDate, deliveryBoy);
				} else {
					paymentDetailsList = paymentDetailsRepository.findAllByPaidOnGreaterThanEqualAndDeliveryBoy(fromDate, deliveryBoy);
				}
			} else {
				if (toDate != null) {
					paymentDetailsList = paymentDetailsRepository.findAllByPaidOnLessThanEqualAndDeliveryBoy(toDate, deliveryBoy);
				} else {
					paymentDetailsList = paymentDetailsRepository.findAllByDeliveryBoy(deliveryBoy);
				}
			}
		} else {
			Vendor vendor = vendorService.getVendorDetail(vendorId);
			if (fromDate != null) {
				if (toDate != null) {
					paymentDetailsList = paymentDetailsRepository.findAllByPaidOnBetweenAndVendor(fromDate, toDate, vendor);
				} else {
					paymentDetailsList = paymentDetailsRepository.findAllByPaidOnGreaterThanEqualAndVendor(fromDate, vendor);
				}
			} else {
				if (toDate != null) {
					paymentDetailsList = paymentDetailsRepository.findAllByPaidOnLessThanEqualAndVendor(toDate, vendor);
				} else {
					paymentDetailsList = paymentDetailsRepository.findAllByVendor(vendor);
				}
			}
		}
		List<PaymentDetailsResponseDTO> paymentDetailsResponseDTOs = paymentDetailsMapper.toDtos(paymentDetailsList);
		String idHeader;
		String nameHeader;
		String nameData;
		String idData;
		if (deliveryBoyId != null) {
			idHeader = "Delivery Boy Id";
			idData = "deliveryBoyId";
			nameHeader = "Delivery Boy Name";
			nameData = "deliveryBoyName";
		} else {
			idHeader = "Vendor Id";
			idData = "vendorId";
			nameHeader = "Vendor Name";
			nameData = "vendorName";
		}
		final Object[] paymentDetailsHeaderField = new Object[] { idHeader, nameHeader, "Payment Date", "Transaction Id", "No. Of Orders", "Payment Amount" };
		final Object[] paymentDetailsDataField = new Object[] { idData, nameData, "paidOn", "transactionNo", "noOfOrders", "paymentAmount" };
		try {
			exportCSV.writeCSVFile(paymentDetailsResponseDTOs, paymentDetailsDataField, paymentDetailsHeaderField, httpServletResponse);
		} catch (IOException e) {
			throw new FileOperationException(messageByLocaleService.getMessage(EXPORT_FILE_CREATE_ERROR, null));
		}
	}

	@Override
	public void exportVendorPayout(Long vendorId, final Long businessCategoryId, final HttpServletResponse httpServletResponse) throws FileOperationException {
		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(userLogin.getEntityType()) && UserType.VENDOR.name().equals(userLogin.getEntityType())) {
			vendorId = userLogin.getEntityId();
		}
		List<VendorPayoutDTO> vendorPayoutDTOs = paymentDetailsRepository.getVendorPayout(vendorId, businessCategoryId, null, null);
		final Object[] vendorPayoutHeaderField = new Object[] { "Business Name", "Business Contact Number", "Vendor Name", "Vendor Contact Number", "Vendor Id",
				"Business Category", "Registered On", "Cart Orders", "Replacement Orders", "Return Orders", "Total Orders", "Total Paid", "Last Paid On" };
		final Object[] vendorPayoutDataField = new Object[] { "storeName", "storePhoneNumber", "name", "phoneNumber", "id", "businessCategoryName",
				"registeredOn", "cartOrders", "replacementOrders", "returnOrders", "totalAttended", "totalPaid", "lastPaymentOn" };
		try {
			exportCSV.writeCSVFile(vendorPayoutDTOs, vendorPayoutDataField, vendorPayoutHeaderField, httpServletResponse);
		} catch (IOException e) {
			throw new FileOperationException(messageByLocaleService.getMessage(EXPORT_FILE_CREATE_ERROR, null));
		}
	}

	@Override
	public void exportDeliveryBoyPayout(final Long deliveryBoyId, final Date registeredOn, final HttpServletResponse httpServletResponse)
			throws FileOperationException {
		List<DeliveryBoyPayoutDTO> deliveryBoyPayoutDTOs = paymentDetailsRepository.getDeliveryBoyPayout(deliveryBoyId, registeredOn, null, null);
		final Object[] deliveryBoyPayoutHeaderField = new Object[] { "Delivery Boy Name", "Delivery Boy Contact Number", "Delivery Boy Id", "Registered On",
				"Cart Orders", "Replacement Orders", "Return Orders", "Total Orders", "Total Paid", "Last Paid On" };
		final Object[] deliveryBoyPayoutDataField = new Object[] { "name", "phoneNumber", "id", "registeredOn", "cartOrders", "replacementOrders",
				"returnOrders", "totalAttended", "totalPaid", "lastPaymentOn" };
		try {
			exportCSV.writeCSVFile(deliveryBoyPayoutDTOs, deliveryBoyPayoutDataField, deliveryBoyPayoutHeaderField, httpServletResponse);
		} catch (IOException e) {
			throw new FileOperationException(messageByLocaleService.getMessage(EXPORT_FILE_CREATE_ERROR, null));
		}
	}

	@Override
	public void sendEmailAfterPayout(final String entityType, final Long paymentDetailsId) throws NotFoundException {
		PaymentDetails paymentDetails = getPaymentDetailsDetail(paymentDetailsId);
		Notification notification = new Notification();
		if (UserType.DELIVERY_BOY.name().equals(entityType)) {
			notification.setDeliveryBoyId(paymentDetails.getDeliveryBoy().getId());
		} else {
			notification.setVendorId(paymentDetails.getVendor().getId());
		}
		notification.setPaymentDetailsId(paymentDetails.getId());
		notification.setType(NotificationQueueConstants.PAYOUT);
		jmsQueuerService.sendEmail(NotificationQueueConstants.GENERAL_QUEUE, notification);
	}

	@Override
	public void sendPushNotificationAfterPayout(final String entityType, final Long paymentDetailsId) throws NotFoundException {
		PaymentDetails paymentDetails = getPaymentDetailsDetail(paymentDetailsId);
		PushNotificationDTO pushNotificationDTO = new PushNotificationDTO();
		if (UserType.DELIVERY_BOY.name().equals(entityType)) {
			pushNotificationDTO.setDeliveryBoyId(paymentDetails.getDeliveryBoy().getId());
			pushNotificationDTO.setLanguage(paymentDetails.getDeliveryBoy().getPreferredLanguage());
		} else {
			pushNotificationDTO.setVendorId(paymentDetails.getVendor().getId());
			pushNotificationDTO.setLanguage(paymentDetails.getVendor().getPreferredLanguage());
		}
		pushNotificationDTO.setPaymentDetailsId(paymentDetailsId);
		pushNotificationDTO.setType(NotificationQueueConstants.PAYOUT);
		jmsQueuerService.sendPushNotification(NotificationQueueConstants.GENERAL_PUSH_NOTIFICATION_QUEUE, pushNotificationDTO);
	}
}
