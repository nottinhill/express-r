require(XML)
require(xml2)
extractPainFile <- function(file) {
  doc = xmlParse(file)
  payments = xpathApply(doc,'//*[name()="PmtInf"]')
  list(
       msgId = xmlValue(xpathApply(doc,'//*[name()="MsgId"]/text()')[[1]]),
       payments = lapply(payments, function(pmtinf) {
                                                    transfers = xpathApply(pmtinf,'*[contains(name(),"TxInf")]')
                                                    values = function(path) 
                                                    as.character(sapply(transfers, function(node) xmlValue(xpathApply(node,path)[[1]])))
                                                    list(
                                                         paymentId = xmlValue(xpathApply(pmtinf,'*[name()="PmtInfId"]/text()')[[1]]),
                                                         transfers = data.frame(
                                                                                TME   = values('*[name()="PmtId"]/*[name()="EndToEndId"]/text()'),
                                                                                amount= values('.//*[name()="InstdAmt"]/text()'),
                                                                                name  = values('.//*[name()="Nm"]/text()'),
                                                                                iban  = values('.//*[name()="IBAN"]/text()')
                                                                                )
                                                         )
                                                    }
                        )
       ) 
}

createCAMTFiles <- function(painInfo, camt53File) {
  if(length(grep("pain-008", painInfo$filename)) > 0){
    typ <- "Debit"
  }else if(length(grep("pain-001", painInfo$filename)) > 0){
    typ <- "Credit"
  }
  fileConn <- file(camt53File)
  writeLines(c(
    '<?xml version="1.0" encoding="UTF-8" ?>',
    '<Document xmlns="urn:iso:std:iso:20022:tech:xsd:camt.053.001.02" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="urn:iso:std:iso:20022:tech:xsd:camt.053.001.02 camt.053.001.02.xsd">',
    '  <BkToCstmrStmt>',
    '    <GrpHdr>',
    '      <MsgId>TestMsgId1</MsgId>',
    '      <CreDtTm>2018-01-16T20:02:58.5+01:00</CreDtTm>',
    '      <MsgRcpt>',
    '        <Id>',
    '         <OrgId>',
    '            <Othr>',
    '              <Id>20766427</Id>',
    '            </Othr>',
    '          </OrgId>',
    '        </Id>',
    '      </MsgRcpt>',
    '      <MsgPgntn>',
    '        <PgNb>1</PgNb>',
    '        <LastPgInd>true</LastPgInd>',
    '      </MsgPgntn>',
    '    </GrpHdr>',
    '    <Stmt>',
    '      <Id>123456789+2018+01+16</Id>',
    '      <ElctrncSeqNb>2</ElctrncSeqNb>',
    '      <CreDtTm>2018-01-16T20:02:58.5+01:00</CreDtTm>',
    '      <FrToDt>',
    '        <FrDtTm>2018-01-16T00:00:00.0+01:00</FrDtTm>',
    '        <ToDtTm>2018-01-16T23:59:59.9+01:00</ToDtTm>',
    '      </FrToDt>',
    '      <Acct>',
    '        <Id>',
    '          <IBAN>DE04700500000004687085</IBAN>',
    '        </Id>',
    '        <Ccy>EUR</Ccy>',
    '        <Svcr>',
    '          <FinInstnId>',
    '            <BIC>BYLADEMMXXX</BIC>',
    '            <Othr>',
    '              <Id>DE129273371</Id>',
    '              <Issr>UmsStId</Issr>',
    '            </Othr>',
    '          </FinInstnId>',
    '        </Svcr>',
    '      </Acct>',
    '      <Bal>',
    '        <Tp>',
    '          <CdOrPrtry>',
    '            <Cd>PRCD</Cd>',
    '          </CdOrPrtry>',
    '        </Tp>',
    '        <Amt Ccy="EUR">123456.78</Amt>',
    '        <CdtDbtInd>CRDT</CdtDbtInd>',
    '        <Dt>',
    '          <Dt>2018-01-03</Dt>',
    '        </Dt>',
    '      </Bal>',
    '      <Bal>',
    '        <Tp>',
    '          <CdOrPrtry>',
    '            <Cd>CLBD</Cd>',
    '          </CdOrPrtry>',
    '        </Tp>',
    '        <Amt Ccy="EUR">12345.67</Amt>',
    '        <CdtDbtInd>CRDT</CdtDbtInd>',
    '        <Dt>',
    '          <Dt>2018-01-16</Dt>',
    '        </Dt>',
    '      </Bal>',
    sapply(painInfo$payments, function(pmt) { c(
      '      <Ntry>', paste0(
        '      <Amt Ccy="EUR">', sum(as.numeric(paste(pmt$transfers[,2]))), '</Amt>'),
      if(typ == "Debit"){
        '      <CdtDbtInd>CRDT</CdtDbtInd>'
      }else if(typ == "Credit"){
        '      <CdtDbtInd>DBIT</CdtDbtInd>'
      }
      ,'      <RvslInd>false</RvslInd>',
      '      <Sts>BOOK</Sts>',
      '      <BookgDt>',
      '        <Dt>2017-08-24</Dt>',
      '      </BookgDt>',
      '      <ValDt>',
      '        <Dt>2017-08-24</Dt>',
      '      </ValDt>',
      '      <AcctSvcrRef>1705</AcctSvcrRef>',
      '      <BkTxCd>',
      '      </BkTxCd>',
      '      <NtryDtls>',
      '        <Btch>', paste0(
        '          <PmtInfId>', pmt$paymentId,'</PmtInfId>'),
      '        </Btch>',
      '        <TxDtls>',
      '          <BkTxCd>',
      '            <Prtry>',
      '              <Cd>NTRF+191+1705</Cd>',
      '              <Issr>ZKA</Issr>',
      '            </Prtry>',
      '          </BkTxCd>',
      '          <RmtInf>',
      '            <Ustrd>ANZ. SEPA     0000002 REF. 599D70AD99594A15623BBD 01.2017-08-24 EINR.ART. SCT VORMERKREF. 9365-08-24-15.0</Ustrd>',
      '            <Ustrd>3.24.730937</Ustrd>',
      '          </RmtInf>',
      '        </TxDtls>',
      '      </NtryDtls>',
      if(typ == "Debit"){
        '      <AddtlNtryInf>SEPA-LASTSCHRIFT-CORE AUSG</AddtlNtryInf>'  
      }else if(typ == "Credit"){
        '      <AddtlNtryInf>SEPA-UEBERWEISUNG-AUSGANG</AddtlNtryInf>'  
      }
      ,'     </Ntry>')}),
    '    </Stmt>',
    '   </BkToCstmrStmt>',
    '</Document>'), fileConn)
  close(fileConn)
}

# Status Files dienen für MSV zur Bestätigung payments.
# Anhand des MsgId werden die TME's aller zugehörigen PmtInfs bestätigt. 
createStatusFiles <- function(painInfo, StatusFile) {
  fileConn = file(StatusFile,"w")
  InfoMsgId=paste0(painInfo$filename, ".20171114093844101")
  GrpSts = "RCVD"
  if(grepl("DD__", painInfo$filename)){
    InfoMsgId=painInfo$msgId
    GrpSts = "PDNG"
  }
  
  writeLines(c(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<Document xsi:schemaLocation="urn:iso:std:iso:20022:tech:xsd:pain.002.001.03" xmlns="urn:iso:std:iso:20022:tech:xsd:pain.002.001.03" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">',
    '    <CstmrPmtStsRpt>',
    '        <GrpHdr>',
    '            <MsgId>1234</MsgId>',
    '            <CreDtTm>2017-08-02T17:05:06.000</CreDtTm>',
    '        </GrpHdr>', 
    '        <OrgnlGrpInfAndSts>'), con=fileConn)
  writeLines(c(
    '            <OrgnlMsgId>', paste0(InfoMsgId), '</OrgnlMsgId>'), con=fileConn, sep="")
  writeLines(c('',
    '            <OrgnlMsgNmId>pain.002.001.03</OrgnlMsgNmId>',
    '            <OrgnlCreDtTm>2017-11-15T13:27:58.120Z</OrgnlCreDtTm>',
    '            <OrgnlNbOfTxs>1</OrgnlNbOfTxs>',
    '            <OrgnlCtrlSum>1</OrgnlCtrlSum>'), con=fileConn)
  writeLines(c(
    '            <GrpSts>', paste0(GrpSts), '</GrpSts>'), con=fileConn, sep="")
  writeLines(c('',
    '        </OrgnlGrpInfAndSts>',
    '    </CstmrPmtStsRpt>',
    '</Document>'), con=fileConn)
    close(fileConn)
}

# Rejection Files get used for MSV payment rejections
# Rejections can take place according to status information on three levels:
# - GrpSts: First Level, all Payments (TME) are rejected. -> RejGrpStsInf = 1
# - PmtInf: Second Level, all TME's from this block are rejected. -> RejPmtInfStsInf=list(1,...)
# - TxSts:  Third Level, specific TME's are rejected. -> RejTxStsInf<-list(), RejTxStsInf[[1]]<-list(1,...),...
# According to EndtoEndId (that held the information about the TMEs), single TME's get rejected. 
createRejectionFiles <- function(painInfo, StatusFile, RejTxStsInf, RejPmtInfStsInf, RejGrpStsInf, ReasonCode = "AC01") {
  GrpSts = "PART"
  if(RejGrpStsInf)
  {
    GrpSts = "RJCT" 
  }
  
  fileConn = file(StatusFile,"w") 
  
  writeLines(c(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<Document xsi:schemaLocation="urn:iso:std:iso:20022:tech:xsd:pain.002.001.03" xmlns="urn:iso:std:iso:20022:tech:xsd:pain.002.001.03" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">',
    '  <CstmrPmtStsRpt>',
    '     <GrpHdr>',
    '         <MsgId>1234</MsgId>',
    '         <CreDtTm>2017-08-02T17:05:06.000</CreDtTm>',
    '     </GrpHdr>', 
    #
    # Start Group Level
    #
    '     <OrgnlGrpInfAndSts>'), con=fileConn)
  writeLines(c(
    '         <OrgnlMsgId>', paste0(painInfo$msgId), '</OrgnlMsgId>'), con=fileConn, sep="")
  writeLines(c('',
    '         <OrgnlMsgNmId>pain.002.001.03</OrgnlMsgNmId>',
    '         <OrgnlCreDtTm>2017-11-15T13:27:58.120Z</OrgnlCreDtTm>',
    '         <OrgnlNbOfTxs>1</OrgnlNbOfTxs>',
    '         <OrgnlCtrlSum>1</OrgnlCtrlSum>'), con=fileConn)
  writeLines(c(
    '         <GrpSts>', paste0(GrpSts), '</GrpSts>'), con=fileConn, sep="")
  writeLines(c('',
    '     </OrgnlGrpInfAndSts>'), con=fileConn)
    #
    # End Group Level
    #
    #
    # Start Payment Level (only if group is not rejected)
    #
  for(i in 1:length(painInfo$payments)){
    if(!RejGrpStsInf){
      PmtInfSts = 'PART'
      if(RejPmtInfStsInf[[i]]){
        PmtInfSts = 'RJCT'
      }
      writeLines(c(
    '     <OrgnlPmtInfAndSts>'), con=fileConn)
      writeLines(c(
    '         <OrgnlPmtInfId>', painInfo$payments[[i]]$paymentId, '</OrgnlPmtInfId>'), con=fileConn, sep="")
      writeLines(c("",
    '         <OrgnlNbOfTxs>2</OrgnlNbOfTxs>', 
    '         <OrgnlCtrlSum>2</OrgnlCtrlSum>'), con=fileConn)
      writeLines(c(
    '         <PmtInfSts>', paste0(PmtInfSts), '</PmtInfSts>'), con=fileConn, sep="")
      writeLines(c(''), con=fileConn)
    }
    #
    # Start Transaction (Tx) Level (only if payment information is not rejected)
    # 
    for(j in 1:dim(painInfo$payments[[i]]$transfers)[1]){
      EndtoEndId=painInfo$payments[[i]]$transfers[j,1]
      Amount=painInfo$payments[[i]]$transfers[j,2]
      if(!RejPmtInfStsInf[[i]]&RejTxStsInf[[i]][[j]]){
        writeLines(c(
    '         <TxInfAndSts>'), con=fileConn)
        writeLines(c(
    '          <StsId>', paste0(EndtoEndId), '</StsId>\n', 
    '          <OrgnlEndToEndId>', paste0(EndtoEndId), '</OrgnlEndToEndId>\n',
    '          <TxSts>RJCT</TxSts>'), con=fileConn, sep="")
        writeLines(c('',
    '          <StsRsnInf>', 
    '          	<Orgtr>', 
    '		 			  	<Id>', 
    '		 		  			<OrgId>', 
    '		 	  					<BICOrBEI>VALLMTMTXXX</BICOrBEI>', 
    '		   					</OrgId>', 
    '	  	 				</Id>', 
    '  		 			</Orgtr>', 
    '			 		  <Rsn>'), con = fileConn)
    writeLines(c(
    paste0('              <Cd>', ReasonCode, '</Cd>', sep = "")), con = fileConn)
    writeLines(c(
    '			  		</Rsn>', 
    '		  		</StsRsnInf>', 
    '	  			<AccptncDtTm>2017-11-14T10:50:01</AccptncDtTm>', 
    '  				<OrgnlTxRef>'), con=fileConn)
        writeLines(c(
    '				  	<Amt><InstdAmt Ccy="EUR">', paste0(Amount), '</InstdAmt></Amt>'), con=fileConn, sep="") 
    writeLines(c('',
    '			  		<ReqdColltnDt>2017-11-15</ReqdColltnDt>', 
    '		  			<PmtTpInf>', 
    '	  					<SvcLvl>', 
    '  							<Cd>SEPA</Cd>', 
    '					  	</SvcLvl>', 
    '				  		<LclInstrm>', 
    '			  				<Cd>CORE</Cd>', 
    '		  				</LclInstrm>', 
    '	  					<SeqTp>OOFF</SeqTp>', 
    '  					</PmtTpInf>', 
    '					  <MndtRltdInf>', 
    '				  		<MndtId>P-234 2017-11-10</MndtId>', 
    '			  			<DtOfSgntr>2017-11-10</DtOfSgntr>', 
    '	  				</MndtRltdInf>', 
    '  					<RmtInf>', 
    '					  	<Ustrd>SavviSave P-52-S-1</Ustrd>', 
    '				  	</RmtInf>', 
    '			  		<Dbtr><Nm>Max Mustermann</Nm></Dbtr>', 
    '		  			<DbtrAcct>', 
    '	  					<Id>', 
    '  							<IBAN>DE31380707240463399999</IBAN>', 
    '					  	</Id>', 
    '				  	</DbtrAcct>', 
    '			  		<DbtrAgt>', 
    '					  	<FinInstnId>', 
    '				   			<BIC>DEUTDEDBXXX</BIC>', 
    '			  			</FinInstnId>', 
    '		  			</DbtrAgt>', 
    '	  				<CdtrAgt>', 
    '  						<FinInstnId>', 
    '					  		<BIC>VALLMTMTXXX</BIC>', 
    '				  		</FinInstnId>', 
    '			  		</CdtrAgt>', 
    '		  			<CdtrAcct>', 
    '	  					<Id>', 
    '  							<IBAN>MT66VALL22013000000040013563400</IBAN>', 
    '			  			</Id>', 
    '		  			</CdtrAcct>', 
    '	  			</OrgnlTxRef>', 
    '       </TxInfAndSts>'), con=fileConn) 
      }
    }
    #
    # End Transaction (Tx) Level 
    # 
    if(!RejGrpStsInf){
      writeLines(c(
    '     </OrgnlPmtInfAndSts>'), con=fileConn)
    }
    #
    # End Payment Level
    #
  }
  writeLines(c(
    '  </CstmrPmtStsRpt>',
    '</Document>'), con=fileConn)
  close(fileConn)
}


createCAMT54Files <- function(painInfo, RejTME, camt54File, type, ReasonCode = "AC01") {
  fileConn <- file(camt54File, "w")
  writeLines(c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<Document xmlns="urn:iso:std:iso:20022:tech:xsd:camt.054.001.02" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">',
    '  <BkToCstmrDbtCdtNtfctn>',
    '    <GrpHdr>',
    '      <MsgId>TestMsgId1</MsgId>',
    '      <CreDtTm>2017-10-16T19:00:08.576+00:00</CreDtTm>',
    '      <MsgPgntn>',
    '        <PgNb>1</PgNb>',
    '        <LastPgInd>true</LastPgInd>',
    '      </MsgPgntn>',
    '   </GrpHdr>',
    '    <Ntfctn>',
    '      <Id>0004687085+2017+10+16</Id>',
    '      <CreDtTm>2017-10-16T19:00:08.576+00:00</CreDtTm>',
    '      <Acct>',
    '        <Id>',
    '          <IBAN>DE04700500000004687085</IBAN>',
    '        </Id>',
    '        <Tp>',
    '          <Cd>CACC</Cd>',
    '        </Tp>',
    '        <Ccy>EUR</Ccy>',
    '        <Nm>Munich RE PCC Limited</Nm>',
    '        <Svcr>',
    '          <FinInstnId>',
    '            <BIC>BYLADEMMXXX</BIC>',
    '            <Nm>Bayerische Landesbank</Nm>',
    '            <Othr>',
    '              <Id>DE 1 29 27 33 71</Id>',
    '              <Issr>UmsStId</Issr>',
    '            </Othr>',
    '          </FinInstnId>',
    '        </Svcr>',
    '      </Acct>'), con = fileConn)
  for (i in 1:length(RejTME)) {
    if (RejTME[[i]] == 1) {
      writeLines(c(
        '      <Ntry>'), con = fileConn)
      writeLines(c(
        '        <Amt Ccy="EUR">', paste0(painInfo$payments[[1]]$transfers[i, 2]),'</Amt>'), con = fileConn, sep = "")
      if (type == "DD") {
        writeLines(c(
          '        <CdtDbtInd>DBIT</CdtDbtInd>'), con = fileConn)
      } else if (type == "CT") {
        writeLines(c(
          '        <CdtDbtInd>CRDT</CdtDbtInd>'), con = fileConn)
      }
      writeLines(c(
        '        <Sts>BOOK</Sts>',
        '        <BookgDt>',
        '          <Dt>2017-10-16</Dt>',
        '        </BookgDt>',
        '        <ValDt>',
        '          <Dt>2017-10-16</Dt>',
        '        </ValDt>',
        '        <BkTxCd></BkTxCd>'), con = fileConn)
      writeLines(c(
        '        <NtryDtls>',
        '          <Btch>',
        '            <NbOfTxs>', 1, '</NbOfTxs>'), con = fileConn, sep = "")
      writeLines(c(
        '            <TtlAmt Ccy="EUR">', paste0(painInfo$payments[[1]]$transfers[i, 2]), '</TtlAmt>'), con = fileConn, sep = "")
      if (type == "DD") {
        writeLines(c(
          '            <CdtDbtInd>DBIT</CdtDbtInd>',
          '          </Btch>'), con = fileConn)
      } else if (type == "CT") {
        writeLines(c(
          '            <CdtDbtInd>CRDT</CdtDbtInd>',
          '          </Btch>'), con = fileConn)
      }
      writeLines(c(
        '          <TxDtls>',
        '            <Refs>',
        '              <InstrId>BYLADEMMXXXG5D28529136539f4</InstrId>'), con = fileConn)
      writeLines(c(
        '              <EndToEndId>', paste0(painInfo$payments[[1]]$transfers[i, 1]), '</EndToEndId>'), con = fileConn, sep = "")
      writeLines(c(
        '              <TxId>BYLADEMMXXX-G5D28529136539f4</TxId>'), con = fileConn)
      if (type == "DD") {
        writeLines(c(
          '              <MndtId>P-Test</MndtId>'), con = fileConn)
      }
      writeLines(c(
        '            </Refs>',
        '            <AmtDtls>',
        '              <InstdAmt>'), con = fileConn)
      writeLines(c(
        '                <Amt Ccy="EUR">', paste0(painInfo$payments[[1]]$transfers[i, 2]), '</Amt>'), con = fileConn, sep = "")
      writeLines(c(
        '              </InstdAmt>',
        '              <TxAmt>'), con = fileConn)
      writeLines(c(
        '                <Amt Ccy="EUR">', paste0(painInfo$payments[[1]]$transfers[i, 2]), '</Amt>'), con = fileConn, sep = "")
      writeLines(c(
        '              </TxAmt>',
        '            </AmtDtls>',
        '            <BkTxCd>',
        '              <Prtry>',
        '                <Cd>NDDT+109</Cd>',
        '                <Issr>ZKA</Issr>',
        '              </Prtry>',
        '            </BkTxCd>'), con = fileConn)
      if (type == "DD") {
        writeLines(c(
          '            <RltdPties>',
          '              <Dbtr>'), con = fileConn)
        writeLines(c(
          '                <Nm>', paste0(painInfo$payments[[1]]$transfers[i, 3]), '</Nm>'), con = fileConn, sep = "")
        writeLines(c(
          '              </Dbtr>',
          '              <DbtrAcct>',
          '                <Id>'), con = fileConn)
        writeLines(c(
          '                  <IBAN>', paste0(painInfo$payments[[1]]$transfers[i, 4]), '</IBAN>'), con = fileConn, sep = "")
        writeLines(c(
          '                </Id>',
          '              </DbtrAcct>',
          '              <Cdtr>',
          '                <Nm>Munich Re PCC Limited</Nm>',
          '              </Cdtr>',
          '              <CdtrAcct>',
          '                <Id>',
          '                  <IBAN>DE04700500000004687085</IBAN>',
          '                </Id>',
          '              </CdtrAcct>',
          '              <UltmtCdtr>',
          '                <Nm>Munich Re PCC Limited Savings and Investments Cell</Nm>',
          '              </UltmtCdtr>',
          '            </RltdPties>'), con = fileConn)
      } else if (type == "CT") {
        writeLines(c(
          '            <RltdPties>',
          '              <Dbtr>',
          '                <Nm>Munich Re PCC Limited</Nm>',
          '              </Dbtr>',
          '              <DbtrAcct>',
          '                <Id>',
          '                  <IBAN>DE04700500000004687085</IBAN>',
          '                </Id>',
          '              </DbtrAcct>',
          '              <UltmtDbtr>',
          '                <Nm>Munich Re PCC Limited Savings and Investments Cell</Nm>',
          '              </UltmtDbtr>',
          '              <Cdtr>'), con = fileConn)
        writeLines(c(
          '                <Nm>', paste0(painInfo$payments[[1]]$transfers[i, 3]), '</Nm>'), con = fileConn, sep = "")
        writeLines(c(
          '              </Cdtr>',
          '              <CdtrAcct>',
          '                <Id>'),con = fileConn, sep = "")
        writeLines(c(
          '                  <IBAN>', paste0(painInfo$payments[[1]]$transfers[i, 4]), '</IBAN>'), con = fileConn, sep = "")
        writeLines(c(
          '                </Id>',
          '              </CdtrAcct>',
          '            </RltdPties>'), con = fileConn)
      }
      writeLines(c(
        '            <RltdAgts>',
        '              <DbtrAgt>',
        '                <FinInstnId>',
        '                  <BIC>BYLADEMMXXX</BIC>',
        '                </FinInstnId>',
        '              </DbtrAgt>',
        '              <CdtrAgt>',
        '                <FinInstnId>',
        '                  <BIC>BYLADEMMXXX</BIC>',
        '                </FinInstnId>',
        '              </CdtrAgt>',
        '            </RltdAgts>',
        '            <RmtInf>',
        '              <Ustrd>Beitrag für SaveUp Sparziel P-Test</Ustrd>',
        '            </RmtInf>',
        '            <RtrInf><Orgtr>',
        '              <Id>',
        '                <OrgId>',
        '                  <BICOrBEI>BYLADEMMXXX</BICOrBEI>',
        '                </OrgId>',
        '              </Id>',
        '            </Orgtr>',
        '            <Rsn>'), con = fileConn)
        writeLines(c(
        paste0('              <Cd>', ReasonCode, '</Cd>', sep = "")), con = fileConn)
        writeLines(c(
        '            </Rsn>',
        '            <AddtlInf>REJECT IBAN FEHLERHAFT</AddtlInf>',
        '            </RtrInf>',
        '          </TxDtls>',
        '        </NtryDtls>'), con = fileConn)
      if (type == "DD") {
        writeLines(c(
          '        <AddtlNtryInf>SEPA Direct Debit (Sammler-Soll, Core)</AddtlNtryInf>'), con = fileConn)
      } else if (type == "CT") {
        writeLines(c(
          '        <AddtlNtryInf>SEPA Credit Transfer (Sammler-Haben)</AddtlNtryInf>'), con = fileConn)
      }
      writeLines(c(
        '      </Ntry>'), con = fileConn)
    }
  }
  writeLines(c(
    '    </Ntfctn>',
    '  </BkToCstmrDbtCdtNtfctn>',
    '</Document>'), con = fileConn)
  close(fileConn)
}

