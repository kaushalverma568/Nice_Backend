/**
 *
 */
package com.nice.validator;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import com.nice.dto.TaskDto;
import com.nice.service.TaskService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 16-Jul-2020
 */
@Component
public class TaskValidator implements Validator {

	@Autowired
	private TaskService taskService;

	@Override
	public boolean supports(final Class<?> clazz) {
		return TaskDto.class.equals(clazz);
	}

	@Override
	public void validate(final Object target, final Errors errors) {
		TaskDto taskDto = (TaskDto) target;
		// TODO
		/**
		 * Implement validation logic here
		 */
	}

}
