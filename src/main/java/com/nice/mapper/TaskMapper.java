/**
 *
 */
package com.nice.mapper;

import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Component;

import com.nice.dto.TaskDto;
import com.nice.dto.TaskResponseDto;
import com.nice.model.Task;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 16-Jul-2020
 */
@Component
public class TaskMapper {

	public Task toEntity(final TaskDto taskDTO) {
		Task task = new Task();
		BeanUtils.copyProperties(taskDTO, task);
		return task;
	}

	public TaskResponseDto toResponseDto(final Task task) {
		TaskResponseDto taskResponseDto = new TaskResponseDto();
		BeanUtils.copyProperties(task, taskResponseDto);
		taskResponseDto.setOrderAmount(task.getOrder().getTotalOrderAmount());
		taskResponseDto.setPaymentMode(task.getOrder().getPaymentMode());
		taskResponseDto.setOrderAmtWithoutDeliveryCharge(task.getOrder().getTotalOrderAmount() - task.getOrder().getDeliveryCharge());
		taskResponseDto.setOrderId(task.getOrder().getId());
		taskResponseDto.setCustomerAddress(task.getOrder().getAddress());
		taskResponseDto.setCustomerName(task.getOrder().getFirstName().concat(" ").concat(task.getOrder().getLastName()));
		taskResponseDto.setCustomerContactNumber(task.getOrder().getPhoneNumber());
		return taskResponseDto;
	}
}
