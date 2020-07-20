/**
 *
 */
package com.nice.constant;

import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 15-Jul-2020
 */
public enum TaskStatusEnum implements BasicStatus<TaskStatusEnum> {
	/**
	 * For Regular Delivery
	 */
	ORDER_ACCEPTED("Order Accepted"), PICK_UP_ON_WAY("Pick Up On Way"), REACHED_VENDOR("Reached Vendor"), ON_THE_WAY("Delivery On The Way"),
	DELIVERED("Delivered"), CANCELLED("Cancelled");

	private String statusValue;

	/**
	 *
	 */
	private TaskStatusEnum(final String value) {
		statusValue = value;
	}

	@Override
	public String getStatusValue() {
		return statusValue;
	}

	private static final Map<String, TaskStatusEnum> TASK_STATUS = new HashMap<>();
	static {
		for (final TaskStatusEnum taskStatus : values()) {
			TASK_STATUS.put(taskStatus.getStatusValue(), taskStatus);
		}
	}

	public static TaskStatusEnum getByValue(final String value) {
		return TASK_STATUS.get(value);
	}

	@Override
	public BasicStatus<TaskStatusEnum>[] nextStatus() {
		TaskStatusEnum[] nextStatus = null;
		switch (this) {
		case ORDER_ACCEPTED:
			nextStatus = new TaskStatusEnum[] { PICK_UP_ON_WAY, CANCELLED };
			break;
		case PICK_UP_ON_WAY:
			nextStatus = new TaskStatusEnum[] { REACHED_VENDOR, CANCELLED };
			break;
		case REACHED_VENDOR:
			nextStatus = new TaskStatusEnum[] { ON_THE_WAY, CANCELLED };
			break;
		case ON_THE_WAY:
			nextStatus = new TaskStatusEnum[] { DELIVERED, CANCELLED };
			break;
		case DELIVERED:
			nextStatus = new TaskStatusEnum[] {};
			break;
		case CANCELLED:
			nextStatus = new TaskStatusEnum[] {};
		}

		return nextStatus;
	}

	public boolean contains(final String newStatus) {
		if (nextStatus() != null) {
			for (final BasicStatus<TaskStatusEnum> status : nextStatus()) {
				if (newStatus.equals(status.getStatusValue())) {
					return true;
				}
			}
		}
		return false;
	}

}
