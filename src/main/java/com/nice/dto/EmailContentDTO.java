package com.nice.dto;

import java.util.List;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 26-Jun-2020
 */
@Data
public class EmailContentDTO {

	private String emailFrom;
	private List<String> emailTo;
	private String emailSubject;
	private String emailBody;
	private List<String> emailBcc;
	private String replyToEmail;
	private List<String> emailCc;
}
