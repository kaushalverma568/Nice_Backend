/**
 *
 */
package com.nice.constant;

import java.util.HashMap;
import java.util.Map;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 29-06-2020
 */
public enum VendorStatus implements BasicStatus<VendorStatus> {

	VERIFICATION_PENDING("Verification_Pending"), NEW("New"), APPROVED("Approved"), REJECTED("Rejected"), ACTIVE("Active"), EXPIRED("Expired"),
	SUSPENDED("Suspended");

	String statusValue;

	VendorStatus(final String statusValue) {
		this.statusValue = statusValue;
	}

	@Override
	public String getStatusValue() {
		return statusValue;
	}

	private static final Map<String, VendorStatus> VENDOR_STATUS = new HashMap<>();
	static {
		for (final VendorStatus orderStatus : values()) {
			VENDOR_STATUS.put(orderStatus.getStatusValue(), orderStatus);
		}
	}

	public static VendorStatus getByValue(final String value) {
		return VENDOR_STATUS.get(value);
	}

	@Override
	public BasicStatus<VendorStatus>[] nextStatus() {
		VendorStatus[] nextStatus = null;
		switch (this) {
		case VERIFICATION_PENDING:
			nextStatus = new VendorStatus[] { NEW, ACTIVE };
			break;
		case NEW:
			nextStatus = new VendorStatus[] { APPROVED, REJECTED };
			break;
		case APPROVED:
			nextStatus = new VendorStatus[] { ACTIVE };
			break;
		case ACTIVE:
			nextStatus = new VendorStatus[] { EXPIRED, SUSPENDED };
			break;
		case EXPIRED:
			nextStatus = new VendorStatus[] { ACTIVE, SUSPENDED };
			break;
		case SUSPENDED:
			nextStatus = new VendorStatus[] { ACTIVE, EXPIRED };
			break;
		case REJECTED:
			nextStatus = new VendorStatus[] {};
			break;
		default:
			break;
		}

		return nextStatus;
	}
}
