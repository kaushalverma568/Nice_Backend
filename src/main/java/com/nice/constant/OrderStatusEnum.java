/**
 *
 */
package com.nice.constant;

import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 09-Jul-2020
 */
public enum OrderStatusEnum implements BasicStatus<OrderStatusEnum> {
	PENDING(Constant.PENDING, Constant.AVAILABLE), IN_PROCESS(Constant.IN_PROCESS, Constant.RESERVED), DELIVERED(Constant.DELIVERED, Constant.DELIVERED),
	REPLACE_REQUESTED(Constant.REPLACE_REQUESTED, Constant.AVAILABLE), REPLACE_PROCESSED(Constant.REPLACE_PROCESSED, Constant.RESERVED),
	REPLACED(Constant.REPLACED, Constant.DELIVERED), CANCELLED(Constant.CANCELLED, Constant.AVAILABLE);

	String statusValue;
	String stockStatus;

	/**
	 *
	 */
	private OrderStatusEnum(final String statusValue, final String stockValue) {
		this.statusValue = statusValue;
		this.stockStatus = stockValue;
	}

	@Override
	public String getStatusValue() {
		return statusValue;
	}

	public String getStockValue() {
		return stockStatus;
	}

	private static final Map<String, OrderStatusEnum> ORDER_STATUS = new HashMap<>();
	static {
		for (final OrderStatusEnum orderStatus : values()) {
			ORDER_STATUS.put(orderStatus.getStatusValue(), orderStatus);
		}
	}

	public static OrderStatusEnum getByValue(final String value) {
		return ORDER_STATUS.get(value);
	}

	@Override
	public BasicStatus<OrderStatusEnum>[] nextStatus() {
		OrderStatusEnum[] nextStatus = null;
		switch (this) {
		case PENDING:
			nextStatus = new OrderStatusEnum[] { IN_PROCESS, CANCELLED };
			break;
		case IN_PROCESS:
			nextStatus = new OrderStatusEnum[] { DELIVERED };
			break;
		case DELIVERED:
			nextStatus = new OrderStatusEnum[] { REPLACE_REQUESTED };
			break;
		case REPLACE_REQUESTED:
			nextStatus = new OrderStatusEnum[] { REPLACE_PROCESSED };
			break;
		case REPLACE_PROCESSED:
			nextStatus = new OrderStatusEnum[] { REPLACED };
			break;
		case REPLACED:
			nextStatus = new OrderStatusEnum[] {};
			break;
		case CANCELLED:
			nextStatus = new OrderStatusEnum[] {};
			break;
		default:
			nextStatus = new OrderStatusEnum[] {};
		}
		return nextStatus;
	}

	public boolean contains(final String newStatus) {
		if (nextStatus() != null) {
			for (final BasicStatus<OrderStatusEnum> status : nextStatus()) {
				if (newStatus.equals(status.getStatusValue())) {
					return true;
				}
			}
		}
		return false;
	}

}