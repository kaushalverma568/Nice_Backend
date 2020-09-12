package com.nice.controller;

import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.oauth2.common.OAuth2AccessToken;
import org.springframework.security.oauth2.provider.token.ConsumerTokenServices;
import org.springframework.security.oauth2.provider.token.TokenStore;
import org.springframework.validation.BindingResult;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.InitBinder;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.nice.constant.Constant;
import com.nice.constant.TaskStatusEnum;
import com.nice.constant.UserType;
import com.nice.dto.AssignedOrdersCountDTO;
import com.nice.dto.DashBoardDetailDTO;
import com.nice.dto.DeliveryBoyAccountDetailsDTO;
import com.nice.dto.DeliveryBoyDTO;
import com.nice.dto.DeliveryBoyFilterDTO;
import com.nice.dto.DeliveryBoyPersonalDetailsDTO;
import com.nice.dto.DeliveryBoyResponseDTO;
import com.nice.dto.OrdersDetailDTOForDeliveryBoy;
import com.nice.dto.OrdersListDTOForDeliveryBoy;
import com.nice.dto.PaginationUtilDto;
import com.nice.dto.TaskFilterDTO;
import com.nice.exception.FileNotFoundException;
import com.nice.exception.FileOperationException;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.DeliveryBoyMapper;
import com.nice.model.DeliveryBoy;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.DeliveryBoyService;
import com.nice.service.TaskService;
import com.nice.util.CommonUtility;
import com.nice.util.PaginationUtil;
import com.nice.validator.DeliveryBoyValidator;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : Jun 18, 2020
 */
@RequestMapping(path = "/deliveryboy")
@RestController
public class DeliveryBoyController {

	private static final String DELIVERYBOY_UPDATE_MESSAGE = "deliveryboy.update.message";
	/*
	 * by logging, display operation detail in console
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(DeliveryBoyController.class);
	private static final String DELIVERYBOY_DETAIL_MESSAGE = "deliveryboy.detail.message";
	/**
	 * Locale message service - to display response messages from messages_en_US.properties
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	/**
	 * Validator - to apply/check any type of validation regarding deliveryBoy
	 */
	@Autowired
	private DeliveryBoyValidator deliveryBoyValidator;

	/**
	 * to bind validator with object using 'BindingResult' in method
	 *
	 * @param binder
	 */
	@InitBinder
	public void initialiseBinder(final WebDataBinder binder) {
		binder.addValidators(deliveryBoyValidator);
	}

	@Autowired
	private DeliveryBoyService deliveryBoyService;

	@Autowired
	private TaskService taskService;

	@Autowired
	private TokenStore tokenStore;

	@Autowired
	private DeliveryBoyMapper deliveryBoyMapper;

	@Autowired
	private ConsumerTokenServices consumerTokenServices;

	/**
	 * Add DeliveryBoy
	 *
	 * @param  deliveryBoyDTO
	 * @param  result
	 * @param  userId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 * @throws FileOperationException
	 * @throws MessagingException
	 */
	@PostMapping
	public ResponseEntity<Object> addDeliveryBoy(@RequestParam(name = "profilePicture", required = false) final MultipartFile profilePicture,
			@ModelAttribute @Valid final DeliveryBoyDTO deliveryBoyDTO, final BindingResult result)
			throws ValidationException, NotFoundException, FileOperationException {
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("DeliveryBoy validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		if (profilePicture == null || !CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(profilePicture.getOriginalFilename())) {
			throw new ValidationException(messageByLocaleService.getMessage("profile.image.required", null));
		}
		deliveryBoyService.addDeliveryBoy(deliveryBoyDTO, profilePicture);
		LOGGER.info("Outside add DeliveryBoy ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("deliveryboy.create.message", null))
				.create();
	}

	/**
	 * Update account details
	 *
	 * @param  deliveryBoyDTO
	 * @param  result
	 * @param  userId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping("/account/details")
	@PreAuthorize("hasPermission('Delivery Boy','CAN_EDIT')")
	public ResponseEntity<Object> updateAccountDetails(@RequestHeader("Authorization") final String accessToken,
			@RequestBody @Valid final DeliveryBoyAccountDetailsDTO deliveryBoyAccountDetailsDTO, final BindingResult result)
			throws ValidationException, NotFoundException {
		LOGGER.info("Inside update account details {}", deliveryBoyAccountDetailsDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("delivery boy validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		deliveryBoyService.updateAccountDetails(deliveryBoyAccountDetailsDTO);
		LOGGER.info("Outside update account details ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(DELIVERYBOY_UPDATE_MESSAGE, null))
				.create();
	}

	/**
	 * Get DeliveryBoy
	 *
	 * @param  deliveryBoyId
	 * @param  userId
	 * @return
	 * @throws NotFoundException
	 */
	@GetMapping("/{deliveryBoyId}")
	public ResponseEntity<Object> getDeliveryBoy(@RequestHeader("Authorization") final String accessToken,
			@PathVariable("deliveryBoyId") final Long deliveryBoyId) throws NotFoundException {
		LOGGER.info("Inside get DeliveryBoy for id:{}", deliveryBoyId);
		final DeliveryBoyResponseDTO resultDeliveryBoyResponseDTO = deliveryBoyService.getDeliveryBoy(deliveryBoyId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(DELIVERYBOY_DETAIL_MESSAGE, null))
				.setData(resultDeliveryBoyResponseDTO).create();
	}

	/**
	 * Get DeliveryBoy List
	 *
	 * @param  accessToken
	 * @param  pageNumber
	 * @param  pageSize
	 * @param  activeRecords
	 * @param  searchKeyword
	 * @param  sortByDirection
	 * @param  sortByField
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@PostMapping("/pageNumber/{pageNumber}/pageSize/{pageSize}")
	public ResponseEntity<Object> getDeliveryBoyList(@RequestHeader("Authorization") final String accessToken, @PathVariable final Integer pageNumber,
			@PathVariable final Integer pageSize, @RequestBody final DeliveryBoyFilterDTO deliveryBoyFilterDTO) throws ValidationException {
		LOGGER.info("Inside get delivery boy List");
		Long totalCount = deliveryBoyService.getDeliveryBoyCountBasedOnParams(deliveryBoyFilterDTO);
		PaginationUtilDto paginationUtilDto = PaginationUtil.calculatePagination(pageNumber, pageSize, totalCount);
		List<DeliveryBoy> deliveryBoyList = deliveryBoyService.getDeliveryBoyListBasedOnParams(paginationUtilDto.getStartIndex(), pageSize,
				deliveryBoyFilterDTO);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("deliveryboy.list.message", null))
				.setData(deliveryBoyMapper.toDtos(deliveryBoyList)).setHasNextPage(paginationUtilDto.getHasNextPage())
				.setHasPreviousPage(paginationUtilDto.getHasPreviousPage()).setTotalPages(paginationUtilDto.getTotalPages().intValue())
				.setPageNumber(paginationUtilDto.getPageNumber()).setTotalCount(totalCount).create();
	}

	/**
	 * Export delivery boy
	 *
	 * @param  accessToken
	 * @param  httpServletResponse
	 * @param  activeRecords
	 * @param  searchKeyword
	 * @return
	 * @throws FileNotFoundException
	 */
	@GetMapping("/export/list")
	public ResponseEntity<Object> exportList(@RequestHeader("Authorization") final String accessToken, final HttpServletResponse httpServletResponse,
			@RequestParam(name = "activeRecords", required = false) final Boolean activeRecords,
			@RequestParam(name = "searchKeyword", required = false) final String searchKeyword) throws FileNotFoundException {
		deliveryBoyService.exportList(activeRecords, searchKeyword, httpServletResponse);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("deliveryboy.list.message", null))
				.create();
	}

	/**
	 * Change Status of DeliveryBoy (Active/DeActive)
	 *
	 * @param  deliveryBoyId
	 * @param  active
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@PutMapping("/status/{deliveryBoyId}")
	@PreAuthorize("hasPermission('Delivery Boy','CAN_DELETE')")
	public ResponseEntity<Object> changeStatus(@RequestHeader("Authorization") final String accessToken,
			@PathVariable("deliveryBoyId") final Long deliveryBoyId, @RequestParam("active") final Boolean active)
			throws NotFoundException, ValidationException {
		LOGGER.info("Inside change status of delivery boy of id {} and status {}", deliveryBoyId, active);
		String userName = deliveryBoyService.changeStatus(deliveryBoyId, active);
		if (userName != null) {
			revokeToken(userName.concat("!!").concat(UserType.DELIVERY_BOY.name()));
		}
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(DELIVERYBOY_UPDATE_MESSAGE, null))
				.create();
	}

	/**
	 * update profile picture of delivery boy
	 *
	 * @param  deliveryBoyId
	 * @param  userId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 * @throws FileOperationException
	 */
	@PutMapping("/profilepicture/{deliveryBoyId}")
	@PreAuthorize("hasPermission('Delivery Boy','CAN_EDIT')")
	public ResponseEntity<Object> updateProfilePicture(@RequestHeader("Authorization") final String accessToken,
			@RequestParam(name = "profilePicture", required = false) final MultipartFile profilePicture,
			@PathVariable("deliveryBoyId") final Long deliveryBoyId) throws NotFoundException, ValidationException, FileOperationException {
		LOGGER.info("Inside update profile picture of delivery boy id:{}", deliveryBoyId);
		if (profilePicture == null || !CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(profilePicture.getOriginalFilename())) {
			throw new ValidationException(messageByLocaleService.getMessage("profile.image.required", null));
		}
		deliveryBoyService.updateProfilePicture(profilePicture, deliveryBoyId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("profile.image.update.message", null)).create();
	}

	/**
	 * Accept order
	 *
	 * @param  accessToken
	 * @param  orderId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@PutMapping("/accept/order/{orderId}")
	@PreAuthorize("hasPermission('Delivery Boy','CAN_EDIT')")
	public ResponseEntity<Object> acceptOrder(@RequestHeader("Authorization") final String accessToken, @PathVariable("orderId") final Long orderId)
			throws NotFoundException, ValidationException {
		LOGGER.info("Inside accept order where order id {}", orderId);
		deliveryBoyService.acceptOrder(orderId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("accept.order.success", null))
				.create();
	}

	/**
	 * revoke token for the user
	 *
	 * @param userName
	 * @param userId
	 */
	private void revokeToken(final String userName) {
		LOGGER.info("Revoking token for user {} ", userName);
		Collection<OAuth2AccessToken> tokens = tokenStore.findTokensByClientIdAndUserName(Constant.CLIENT_ID, userName);
		for (OAuth2AccessToken token : tokens) {
			tokenStore.removeAccessToken(token);
		}
		LOGGER.info("Successfully Revoked token for user {}", userName);
	}

	/**
	 * log out delivery boy
	 *
	 * @param  accessToken
	 * @param  userId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@PutMapping("/logout")
	@PreAuthorize("hasPermission('Delivery Boy','CAN_EDIT')")
	public ResponseEntity<Object> logout(@RequestHeader("Authorization") final String accessToken) throws NotFoundException, ValidationException {
		LOGGER.info("Inside Log out method for delivery boy");
		deliveryBoyService.validateBeforeLogout();
		String tokenValue = accessToken.replace("Bearer", "").trim();
		consumerTokenServices.revokeToken(tokenValue);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("logout.message", null)).create();
	}

	/**
	 * Update personal details
	 *
	 * @param  deliveryBoyDTO
	 * @param  result
	 * @param  userId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping()
	@PreAuthorize("hasPermission('Delivery Boy','CAN_EDIT')")
	public ResponseEntity<Object> updatePersonalDetails(@RequestHeader("Authorization") final String accessToken,
			@RequestBody @Valid final DeliveryBoyPersonalDetailsDTO deliveryBoyPersonalDetailsDTO, final BindingResult result)
			throws ValidationException, NotFoundException {
		LOGGER.info("Inside update personal details {}", deliveryBoyPersonalDetailsDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("delivery boy validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		deliveryBoyService.updatePersonalDetails(deliveryBoyPersonalDetailsDTO);
		LOGGER.info("Outside update personal details ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(DELIVERYBOY_UPDATE_MESSAGE, null))
				.create();
	}

	/**
	 * Get delivered orders count
	 *
	 * @param  deliveryBoyId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@GetMapping("/dashboard/{deliveryBoyId}")
	public ResponseEntity<Object> getDashBoard(@RequestHeader("Authorization") final String accessToken,
			@PathVariable("deliveryBoyId") final Long deliveryBoyId) throws NotFoundException, ValidationException {
		LOGGER.info("Inside get dash board for id:{}", deliveryBoyId);
		final DashBoardDetailDTO assignedOrdersCountDTO = deliveryBoyService.getDashBoard(deliveryBoyId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(DELIVERYBOY_DETAIL_MESSAGE, null))
				.setData(assignedOrdersCountDTO).create();
	}

	/**
	 * Get assigned orders count
	 *
	 * @param  deliveryBoyId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@GetMapping("/order/assigned/{deliveryBoyId}")
	public ResponseEntity<Object> getAssignedOrdersCount(@RequestHeader("Authorization") final String accessToken,
			@PathVariable("deliveryBoyId") final Long deliveryBoyId) throws NotFoundException, ValidationException {
		LOGGER.info("Inside get assigned orders count for id:{}", deliveryBoyId);
		final AssignedOrdersCountDTO assignedOrdersCountDTO = deliveryBoyService.getAssignedOrdersCount(deliveryBoyId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(DELIVERYBOY_DETAIL_MESSAGE, null))
				.setData(assignedOrdersCountDTO).create();
	}

	/**
	 * update is available status for delivering orders
	 *
	 * @param  deliveryBoyId
	 * @param  active
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@PutMapping("/available")
	@PreAuthorize("hasPermission('Delivery Boy','CAN_EDIT')")
	public ResponseEntity<Object> updateIsAvailable(@RequestHeader("Authorization") final String accessToken,
			@RequestParam(name = "isAvailable", required = true) final Boolean isAvailable) throws NotFoundException, ValidationException {
		LOGGER.info("update is available for delivery, isAvailable : {}", isAvailable);
		deliveryBoyService.updateIsAvailable(isAvailable);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(DELIVERYBOY_UPDATE_MESSAGE, null))
				.create();
	}

	/**
	 * Get order detail in accept notification
	 *
	 * @param  accessToken
	 * @param  deliveryBoyId
	 * @param  orderId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@GetMapping("/notification/order/{orderId}")
	public ResponseEntity<Object> getOrderDetailInDeliveryBoyAcceptNotification(@RequestHeader("Authorization") final String accessToken,
			@PathVariable("deliveryBoyId") final Long deliveryBoyId, @PathVariable("orderId") final Long orderId)
			throws NotFoundException, ValidationException {
		LOGGER.info("Inside get order detail in delivery boy accept notification for orderId:{}", orderId);
		final OrdersDetailDTOForDeliveryBoy orderNotificationDTO = deliveryBoyService.getOrderDetailInDeliveryBoyAcceptNotification(orderId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("order.detail.message", null))
				.setData(orderNotificationDTO).create();
	}

	/**
	 * Get order detail for delivery boy (This is used for all the order detail screens except (accept/reject notification screen) )
	 *
	 * @param  accessToken
	 * @param  taskId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@GetMapping("/order/details/task/{taskId}")
	public ResponseEntity<Object> getOrderDetail(@RequestHeader("Authorization") final String accessToken, @PathVariable("taskId") final Long taskId)
			throws NotFoundException, ValidationException {
		LOGGER.info("Inside get order detail for taskId:{}", taskId);
		final OrdersDetailDTOForDeliveryBoy orderDetailsDTO = deliveryBoyService.getOrderDetails(taskId, null);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("order.detail.message", null))
				.setData(orderDetailsDTO).create();
	}

	/**
	 * Get assigned order list for delivery boy
	 *
	 * @param  accessToken
	 * @param  deliveryBoyId
	 * @param  pageNumber
	 * @param  pageSize
	 * @param  orderDate
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@GetMapping("/{deliveryBoyId}/list/order/assigned/pageNumber/{pageNumber}/pageSize/{pageSize}")
	public ResponseEntity<Object> getAssignedOrdersList(@RequestHeader("Authorization") final String accessToken,
			@PathVariable("deliveryBoyId") final Long deliveryBoyId, @PathVariable final Integer pageNumber, @PathVariable final Integer pageSize,
			@RequestParam(value = "orderDate", required = false) final Date orderDate,
			@RequestParam(value = "taskType", required = false) final String taskType) throws NotFoundException, ValidationException {
		LOGGER.info("Inside get assigned orders list for delivery boy id:{}", deliveryBoyId);
		TaskFilterDTO taskFilterDTO = new TaskFilterDTO();
		taskFilterDTO.setOrderDate(orderDate);
		taskFilterDTO.setStatusListNotIn(Arrays.asList(TaskStatusEnum.DELIVERED.getStatusValue(), TaskStatusEnum.CANCELLED.getStatusValue()));
		taskFilterDTO.setDeliveryBoyId(deliveryBoyId);
		taskFilterDTO.setTaskType(taskType);
		Long totalCount = taskService.getTaskCountBasedOnParams(taskFilterDTO);
		PaginationUtilDto paginationUtilDto = PaginationUtil.calculatePagination(pageNumber, pageSize, totalCount);
		final List<OrdersListDTOForDeliveryBoy> orderList = deliveryBoyService.getOrdersList(deliveryBoyId, paginationUtilDto.getStartIndex(), pageSize,
				taskFilterDTO);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("order.list.message", null))
				.setData(orderList).setHasNextPage(paginationUtilDto.getHasNextPage()).setHasPreviousPage(paginationUtilDto.getHasPreviousPage())
				.setTotalPages(paginationUtilDto.getTotalPages().intValue()).setPageNumber(paginationUtilDto.getPageNumber()).setTotalCount(totalCount)
				.create();
	}

	/**
	 * Get today's delivered order list for delivery boy
	 *
	 * @param  accessToken
	 * @param  deliveryBoyId
	 * @param  pageNumber
	 * @param  pageSize
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@GetMapping("/{deliveryBoyId}/list/order/delivered/pageNumber/{pageNumber}/pageSize/{pageSize}")
	public ResponseEntity<Object> getDeliveredOrdersList(@RequestHeader("Authorization") final String accessToken,
			@PathVariable("deliveryBoyId") final Long deliveryBoyId, @PathVariable final Integer pageNumber, @PathVariable final Integer pageSize,
			@RequestParam(value = "taskType", required = false) final String taskType) throws ValidationException, NotFoundException {
		LOGGER.info("Inside get delivered orders list for delivery boy id:{}", deliveryBoyId);
		TaskFilterDTO taskFilterDTO = new TaskFilterDTO();
		taskFilterDTO.setDeliveredDate(new Date());
		taskFilterDTO.setStatusList(Arrays.asList(TaskStatusEnum.DELIVERED.getStatusValue()));
		taskFilterDTO.setDeliveryBoyId(deliveryBoyId);
		taskFilterDTO.setTaskType(taskType);
		Long totalCount = taskService.getTaskCountBasedOnParams(taskFilterDTO);
		PaginationUtilDto paginationUtilDto = PaginationUtil.calculatePagination(pageNumber, pageSize, totalCount);
		final List<OrdersListDTOForDeliveryBoy> orderList = deliveryBoyService.getOrdersList(deliveryBoyId, paginationUtilDto.getStartIndex(), pageSize,
				taskFilterDTO);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("order.list.message", null))
				.setData(orderList).setHasNextPage(paginationUtilDto.getHasNextPage()).setHasPreviousPage(paginationUtilDto.getHasPreviousPage())
				.setTotalPages(paginationUtilDto.getTotalPages().intValue()).setPageNumber(paginationUtilDto.getPageNumber()).setTotalCount(totalCount)
				.create();
	}
}
