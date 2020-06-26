/**
 *
 */
package com.nice.constant;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 17-Jun-2020
 */
@Getter
@AllArgsConstructor
public enum RegisterVia {

	APP("APP"), GOOGLE("GOOGLE"), FACEBOOK("FACEBOOK");

	private String statusValue;

}
