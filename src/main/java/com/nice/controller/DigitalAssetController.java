package com.nice.controller;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.nice.exception.FileNotFoundException;
import com.nice.service.FileStorageService;
import com.nice.util.MyResourceHttpRequestHandler;

/**
 * @author      : Kody Technolab PVT. LTD.
 * @date        : 26-June-2020
 * @description : This class is responsible for fetch actual images based on URL
 */
@RestController
@RequestMapping(path = "/digital_asset")
public class DigitalAssetController {

	@Autowired
	private FileStorageService fileStorageService;

	@Autowired
	private MyResourceHttpRequestHandler handler;

	/**
	 * Fetch Image based on fileName and sub directory
	 *
	 * @param  fileName
	 * @param  subDir
	 * @param  request
	 * @param  response
	 * @throws ServletException
	 * @throws IOException
	 * @throws FileNotFoundException
	 */
	@GetMapping("/downloadFile/{fileName:.+}")
	public void streamFile(@PathVariable final String fileName, @RequestParam(name = "subDir", required = true) final String subDir,
			final HttpServletRequest request, final HttpServletResponse response) throws FileNotFoundException, IOException, ServletException {

		request.setAttribute(MyResourceHttpRequestHandler.ATTR_FILE, fileStorageService.loadFileAsResource(fileName, subDir).getFile());
		handler.handleRequest(request, response);
	}
}
