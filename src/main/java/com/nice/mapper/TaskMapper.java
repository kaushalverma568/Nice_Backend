/**
 *
 */
package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Component;

import com.nice.dto.TaskDto;
import com.nice.dto.TaskPayoutDTO;
import com.nice.dto.TaskResponseDto;
import com.nice.model.Task;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 16-Jul-2020
 */
@Component
public class TaskMapper {

	public Task toEntity(final TaskDto taskDTO) {
		Task task = new Task();
		BeanUtils.copyProperties(taskDTO, task);
		return task;
	}

	public TaskResponseDto toResponseDto(final Task task) {

		String deliveryBoyPreferredLanguage = task.getDeliveryBoy().getPreferredLanguage();
		TaskResponseDto taskResponseDto = new TaskResponseDto();
		BeanUtils.copyProperties(task, taskResponseDto);
		taskResponseDto.setOrderAmount(task.getOrder().getTotalOrderAmount());
		taskResponseDto.setPaymentMode(task.getOrder().getPaymentMode());
		taskResponseDto.setOrderAmtWithoutDeliveryCharge(task.getOrder().getTotalOrderAmount() - task.getOrder().getDeliveryCharge());
		taskResponseDto.setOrderId(task.getOrder().getId());
		if (deliveryBoyPreferredLanguage.equals("en")) {
			taskResponseDto.setCustomerAddress(task.getOrder().getAddressEnglish());
		} else {
			taskResponseDto.setCustomerAddress(task.getOrder().getAddressArabic());
		}

		taskResponseDto.setCustomerName(task.getOrder().getFirstName().concat(" ").concat(task.getOrder().getLastName()));
		taskResponseDto.setCustomerPhoneNumber(task.getOrder().getPhoneNumber());
		return taskResponseDto;
	}

	public TaskPayoutDTO toPayoutDto(final Task task) {
		TaskPayoutDTO taskPayoutDTO = new TaskPayoutDTO();
		BeanUtils.copyProperties(task, taskPayoutDTO);
		taskPayoutDTO.setPaymentMode(task.getOrder().getPaymentMode());
		taskPayoutDTO.setOrderId(task.getOrder().getId());
		taskPayoutDTO.setOrderDate(task.getOrder().getCreatedAt());
		taskPayoutDTO.setOrderStatus(task.getOrder().getOrderStatus());
		/**
		 * Get Details related to delivery boy Payment, if payment is done
		 */
		if (task.getDeliveryBoyPaymentDetails() != null) {
			taskPayoutDTO.setDeliveryBoyPaymentDetailsId(task.getDeliveryBoyPaymentDetails().getId());
			taskPayoutDTO.setDeliveryBoyTransactionId(task.getDeliveryBoyPaymentDetails().getTransactionNo());
			taskPayoutDTO.setDeliveryBoyPaidOn(task.getDeliveryBoyPaymentDetails().getPaidOn());
		}
		/**
		 * Set details related to vendor payment, if payment done
		 */
		if (task.getVendorPaymentDetails() != null) {
			taskPayoutDTO.setVendorPaymentDetailsId(task.getVendorPaymentDetails().getId());
			taskPayoutDTO.setVendorTransactionId(task.getVendorPaymentDetails().getTransactionNo());
			taskPayoutDTO.setVendorPaidOn(task.getVendorPaymentDetails().getPaidOn());
		}
		return taskPayoutDTO;
	}

	public List<TaskPayoutDTO> toPayoutResponseDtos(final List<Task> resulttasks) {
		List<TaskPayoutDTO> results = new ArrayList<>();
		for (Task task : resulttasks) {
			results.add(toPayoutDto(task));
		}
		return results;
	}
}
