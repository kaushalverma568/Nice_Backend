/**
 *
 */
package com.nice.constant;

import java.util.HashMap;
import java.util.Map;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 15-Jul-2020
 */
public enum TaskStatusEnum implements BasicStatus<TaskStatusEnum> {
	/**
	 * For Regular Delivery
	 */
	ORDER_ACCEPTED("Order Accepted"), PICK_UP_ON_WAY("Pick Up On Way"), REACHED_VENDOR("Reached At Vendor"), ON_THE_WAY("Order On The Way"),
	DELIVERED("Delivered"), CANCELLED("Cancelled"), REACHED_CUSTOMER("Reached At Customer"), RETURN_ON_THE_WAY("Return On The Way"),
	REPLACE_CUSTOMER_PICKUP_ON_THE_WAY("Replace Customer Pickup On The Way"), REPLACE_DELIVERY_ON_THE_WAY("Replace Delivery On The Way");

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
			nextStatus = new TaskStatusEnum[] { PICK_UP_ON_WAY, CANCELLED, ON_THE_WAY };
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
		default:
			nextStatus = new TaskStatusEnum[] {};
		}

		return nextStatus;
	}

	public BasicStatus<TaskStatusEnum>[] nextReturnOrderTaskStatus() {
		TaskStatusEnum[] nextStatus = null;
		switch (this) {
		case ORDER_ACCEPTED:
			nextStatus = new TaskStatusEnum[] { PICK_UP_ON_WAY, DELIVERED };
			break;
		case PICK_UP_ON_WAY:
			nextStatus = new TaskStatusEnum[] { REACHED_CUSTOMER };
			break;
		case REACHED_CUSTOMER:
			nextStatus = new TaskStatusEnum[] { RETURN_ON_THE_WAY };
			break;
		case RETURN_ON_THE_WAY:
			nextStatus = new TaskStatusEnum[] { DELIVERED };
			break;
		default:
			nextStatus = new TaskStatusEnum[] {};
		}

		return nextStatus;
	}

	public BasicStatus<TaskStatusEnum>[] nextReplaceOrderTaskStatus() {
		TaskStatusEnum[] nextStatus = null;
		switch (this) {
		case ORDER_ACCEPTED:
			nextStatus = new TaskStatusEnum[] { REPLACE_CUSTOMER_PICKUP_ON_THE_WAY, DELIVERED };
			break;
		case REPLACE_CUSTOMER_PICKUP_ON_THE_WAY:
			nextStatus = new TaskStatusEnum[] { REACHED_CUSTOMER };
			break;
		case REACHED_CUSTOMER:
			nextStatus = new TaskStatusEnum[] { PICK_UP_ON_WAY };
			break;
		case PICK_UP_ON_WAY:
			nextStatus = new TaskStatusEnum[] { REACHED_VENDOR };
			break;
		case REACHED_VENDOR:
			nextStatus = new TaskStatusEnum[] { REPLACE_DELIVERY_ON_THE_WAY };
			break;
		case REPLACE_DELIVERY_ON_THE_WAY:
			nextStatus = new TaskStatusEnum[] { DELIVERED };
			break;
		default:
			nextStatus = new TaskStatusEnum[] {};
		}

		return nextStatus;
	}

	public boolean contains(final String newStatus) {
		for (final BasicStatus<TaskStatusEnum> status : nextStatus()) {
			if (newStatus.equals(status.getStatusValue())) {
				return true;
			}
		}
		return false;
	}

	public boolean returnContains(final String newStatus) {
		for (final BasicStatus<TaskStatusEnum> status : nextReturnOrderTaskStatus()) {
			if (newStatus.equals(status.getStatusValue())) {
				return true;
			}
		}
		return false;
	}

	public boolean replaceContains(final String newStatus) {
		for (final BasicStatus<TaskStatusEnum> status : nextReplaceOrderTaskStatus()) {
			if (newStatus.equals(status.getStatusValue())) {
				return true;
			}
		}
		return false;
	}

}
