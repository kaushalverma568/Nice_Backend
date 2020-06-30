/**
 *
 */
package com.nice.constant;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 29-06-2020
 */
public interface BasicStatus<T extends Enum<?>> {

	String getStatusValue();

	BasicStatus<T>[] nextStatus();

}
