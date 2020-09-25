/**
 *
 */
package com.nice.constant;

import java.util.HashMap;
import java.util.Map;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 09-Jul-2020
 */
public enum OrderStatusEnum implements BasicStatus<OrderStatusEnum> {
	PENDING(Constant.PENDING, Constant.AVAILABLE), REJECTED(Constant.REJECTED, Constant.AVAILABLE), CONFIRMED(Constant.CONFIRMED, Constant.AVAILABLE),
	IN_PROCESS(Constant.IN_PROCESS, Constant.AVAILABLE), ORDER_IS_PREPARED(Constant.ORDER_IS_PREPARED, Constant.AVAILABLE),
	WAITING_FOR_PICKUP(Constant.WAITING_FOR_PICKUP, Constant.RESERVED), ORDER_PICKED_UP(Constant.ORDER_PICKED_UP, Constant.RESERVED),
	DELIVERED(Constant.DELIVERED, Constant.DELIVERED),

	REPLACE_REQUESTED(Constant.REPLACE_REQUESTED, Constant.AVAILABLE), REPLACE_CONFIRMED(Constant.REPLACE_CONFIRMED, Constant.AVAILABLE),
	REPLACE_REJECTED(Constant.REPLACE_REJECTED, Constant.AVAILABLE), REPLACE_PROCESSED(Constant.REPLACE_PROCESSED, Constant.RESERVED),
	REPLACE_ORDER_PREPARED(Constant.REPLACE_ORDER_PREPARED, Constant.RESERVED), REPLACE_ORDER_PICKUP(Constant.REPLACE_ORDER_PICKUP, Constant.AVAILABLE),
	REPLACE_WAITING_FOR_PICKUP(Constant.REPLACE_WAITING_FOR_PICKUP, Constant.RESERVED), REPLACED(Constant.REPLACED, Constant.DELIVERED),
	CANCELLED(Constant.CANCELLED, Constant.AVAILABLE),

	RETURN_REQUESTED(Constant.RETURN_REQUESTED, Constant.DELIVERED), RETURN_CONFIRMED(Constant.RETURN_CONFIRMED, Constant.DELIVERED),
	RETURN_REJECTED(Constant.RETURN_REJECTED, Constant.DELIVERED), RETURN_PROCESSED(Constant.RETURN_PROCESSED, Constant.DELIVERED),
	RETURN_ORDER_PICKUP(Constant.RETURN_ORDER_PICKUP, Constant.DELIVERED), RETURNED(Constant.RETURNED, Constant.RETURNED);

	String statusValue;
	String stockStatus;

	/**
	 *
	 */
	private OrderStatusEnum(final String statusValue, final String stockValue) {
		this.statusValue = statusValue;
		stockStatus = stockValue;
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
			nextStatus = new OrderStatusEnum[] { CONFIRMED, CANCELLED, REJECTED };
			break;
		case CONFIRMED:
			nextStatus = new OrderStatusEnum[] { IN_PROCESS };
			break;
		case IN_PROCESS:
			nextStatus = new OrderStatusEnum[] { ORDER_IS_PREPARED };
			break;
		case ORDER_IS_PREPARED:
			nextStatus = new OrderStatusEnum[] { WAITING_FOR_PICKUP };
			break;
		case WAITING_FOR_PICKUP:
			nextStatus = new OrderStatusEnum[] { ORDER_PICKED_UP };
			break;
		case ORDER_PICKED_UP:
			nextStatus = new OrderStatusEnum[] { DELIVERED };
			break;
		case DELIVERED:
			nextStatus = new OrderStatusEnum[] { REPLACE_REQUESTED, RETURN_REQUESTED };
			break;
		case REPLACE_REQUESTED:
			nextStatus = new OrderStatusEnum[] { REPLACE_CONFIRMED, REPLACE_REJECTED };
			break;
		case REPLACE_CONFIRMED:
			nextStatus = new OrderStatusEnum[] { REPLACE_PROCESSED };
			break;
		case REPLACE_PROCESSED:
			nextStatus = new OrderStatusEnum[] { REPLACE_ORDER_PREPARED };
			break;
		case REPLACE_ORDER_PREPARED:
			nextStatus = new OrderStatusEnum[] { REPLACE_ORDER_PICKUP };
			break;
		case REPLACE_ORDER_PICKUP:
			nextStatus = new OrderStatusEnum[] { REPLACE_WAITING_FOR_PICKUP };
			break;
		case REPLACE_WAITING_FOR_PICKUP:
			nextStatus = new OrderStatusEnum[] { REPLACED };
			break;
		case RETURN_REQUESTED:
			nextStatus = new OrderStatusEnum[] { RETURN_CONFIRMED, RETURN_REJECTED };
			break;
		case RETURN_CONFIRMED:
			nextStatus = new OrderStatusEnum[] { RETURN_PROCESSED };
			break;
		case RETURN_PROCESSED:
			nextStatus = new OrderStatusEnum[] { RETURN_ORDER_PICKUP, RETURNED };
			break;
		case RETURN_ORDER_PICKUP:
			nextStatus = new OrderStatusEnum[] { RETURNED };
			break;
		default:
			nextStatus = new OrderStatusEnum[] {};
		}
		return nextStatus;
	}

	public boolean contains(final String newStatus) {
		for (final BasicStatus<OrderStatusEnum> status : nextStatus()) {
			if (newStatus.equals(status.getStatusValue())) {
				return true;
			}
		}
		return false;
	}

}
