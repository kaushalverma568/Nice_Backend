/**
 *
 */
package com.nice.constant;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 24-Jul-2020
 */
public interface BasicStatus<T extends Enum<?>> {

	String getStatusValue();

	BasicStatus<T>[] nextStatus();

}
