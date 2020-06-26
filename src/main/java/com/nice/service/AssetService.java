/**
 *
 */
package com.nice.service;

import org.springframework.web.multipart.MultipartFile;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 26-Jun-2020
 */
public interface AssetService {

	/**
	 * @param  image
	 * @param  subDirectory
	 * @param  count
	 * @return
	 */
	String saveAsset(MultipartFile image, String subDirectory, int count);

}
