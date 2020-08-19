unit EFatura_Class_;

interface

uses
    System.Generics.Collections
  ;

type
  TInvoiceType = class
    private
      FAccountingCustomerParty: TAccountingCustomerPartyType;
      FBillingReference: TArray<TBillingReferenceType>;
      FIssueDate: TIssueDateType;
      FPaymentMeans: TArray<TPaymentMeansType>;
      FAdditionalDocumentReference: TArray<TAdditionalDocumentReferenceType>;
      FDespatchDocumentReference: TArray<TDespatchDocumentReferenceType>;
      FProfileID: TProfileIDType;
      FPaymentExchangeRate: TPaymentExchangeRateType;
      FTaxTotal: TArray<TTaxTotalType>;
      FLegalMonetaryTotal: TLegalMonetaryTotalType;
      FPricingExchangeRate: TPricingExchangeRateType;
      FWithholdingTaxTotal: TArray<TWithholdingTaxTotalType>;
      FPaymentTerms: TPaymentTermsType;
      FOrderReference: TOrderReferenceType;
      FPaymentAlternativeExchangeRate: TPaymentAlternativeExchangeRateType;
      FAccountingSupplierParty: TAccountingSupplierPartyType;
      FUUID: TUUIDType;
      FInvoicePeriod: TInvoicePeriodType;
      FAccountingCost: TAccountingCostType;
      FBuyerCustomerParty: TBuyerCustomerPartyType;
      FPaymentCurrencyCode: TPaymentCurrencyCodeType;
      FID: TIDType;
      FUBLExtensions: TUBLExtensionsType;
      FPricingCurrencyCode: TPricingCurrencyCodeType;
      FDocumentCurrencyCode: TDocumentCurrencyCodeType;
      FIssueTime: TIssueTimeType;
      FSellerSupplierParty: TSellerSupplierPartyType;
      FNote: TArray<TNoteType>;
      FTaxExchangeRate: TTaxExchangeRateType;
      FSignature: TArray<TSignatureType>;
      FInvoiceLine: TArray<TInvoiceLineType>;
      FDelivery: TArray<TDeliveryType>;
      FPaymentAlternativeCurrencyCode: TPaymentAlternativeCurrencyCodeType;
      FCopyIndicator: TCopyIndicatorType;
      FCustomizationID: TCustomizationIDType;
      FOriginatorDocumentReference: TArray<TOriginatorDocumentReferenceType>;
      FLineCountNumeric: TLineCountNumericType;
      FContractDocumentReference: TArray<TContractDocumentReferenceType>;
      FInvoiceTypeCode: TInvoiceTypeCodeType;
      FReceiptDocumentReference: TArray<TReceiptDocumentReferenceType>;
      FUBLVersionID: TUBLVersionIDType;
      FAllowanceCharge: TArray<TAllowanceChargeType>;
      FTaxRepresentativeParty: TTaxRepresentativePartyType;
      FTaxCurrencyCode: TTaxCurrencyCodeType;
    public
      [NameSpace('ext')][Zorunlu]
      property UBLExtensions                           : TUBLExtensionsType                       read FUBLExtensions                            write FUBLExtensions;
      [NameSpace('cbc')][Zorunlu]
      property UBLVersionID                            : TUBLVersionIDType                        read FUBLVersionID                             write FUBLVersionID;
      [NameSpace('cbc')][Zorunlu]
      property CustomizationID                         : TCustomizationIDType                     read FCustomizationID                          write FCustomizationID;
      [NameSpace('cbc')][Zorunlu]
      property ProfileID                               : TProfileIDType                           read FProfileID                                write FProfileID;
      [NameSpace('cbc')][Zorunlu]
      property ID                                      : TIDType                                  read FID                                       write FID;
      [NameSpace('cbc')][Zorunlu]
      property CopyIndicator                           : TCopyIndicatorType                       read FCopyIndicator                            write FCopyIndicator;
      [NameSpace('cbc')][Zorunlu]
      property UUID                                    : TUUIDType                                read FUUID                                     write FUUID;
      [NameSpace('cbc')][Zorunlu]
      property IssueDate                               : TIssueDateType                           read FIssueDate                                write FIssueDate;
      [NameSpace('cbc')]
      property IssueTime                               : TIssueTimeType                           read FIssueTime                                write FIssueTime;
      [NameSpace('cbc')][Zorunlu]
      property InvoiceTypeCode                         : TInvoiceTypeCodeType                     read FInvoiceTypeCode                          write FInvoiceTypeCode;
      [NameSpace('cbc')]
      property Note                                    : TArray<TNoteType>                        read FNote                                     write FNote;
      [NameSpace('cbc')][Zorunlu]
      property DocumentCurrencyCode                    : TDocumentCurrencyCodeType                read FDocumentCurrencyCode                     write FDocumentCurrencyCode;
      [NameSpace('cbc')]
      property TaxCurrencyCode                         : TTaxCurrencyCodeType                     read FTaxCurrencyCode                          write FTaxCurrencyCode;
      [NameSpace('cbc')]
      property PricingCurrencyCode                     : TPricingCurrencyCodeType                 read FPricingCurrencyCode                      write FPricingCurrencyCode;
      [NameSpace('cbc')]
      property PaymentCurrencyCode                     : TPaymentCurrencyCodeType                 read FPaymentCurrencyCode                      write FPaymentCurrencyCode;
      [NameSpace('cbc')]
      property PaymentAlternativeCurrencyCode          : TPaymentAlternativeCurrencyCodeType      read FPaymentAlternativeCurrencyCode           write FPaymentAlternativeCurrencyCode;
      [NameSpace('cbc')]
      property AccountingCost                          : TAccountingCostType                      read FAccountingCost                           write FAccountingCost;
      [NameSpace('cbc')][Zorunlu]
      property LineCountNumeric                        : TLineCountNumericType                    read FLineCountNumeric                         write FLineCountNumeric;
      [NameSpace('cac')]
      property InvoicePeriod                           : TInvoicePeriodType                       read FInvoicePeriod                            write FInvoicePeriod;
      [NameSpace('cac')]
      property OrderReference                          : TOrderReferenceType                      read FOrderReference                           write FOrderReference;
      [NameSpace('cac')]
      property BillingReference                        : TArray<TBillingReferenceType>            read FBillingReference                         write FBillingReference;
      [NameSpace('cac')]
      property DespatchDocumentReference               : TArray<TDespatchDocumentReferenceType>   read FDespatchDocumentReference                write FDespatchDocumentReference;
      [NameSpace('cac')]
      property ReceiptDocumentReference                : TArray<TReceiptDocumentReferenceType>    read FReceiptDocumentReference                 write FReceiptDocumentReference;
      [NameSpace('cac')]
      property OriginatorDocumentReference             : TArray<TOriginatorDocumentReferenceType> read FOriginatorDocumentReference              write FOriginatorDocumentReference;
      [NameSpace('cac')]
      property ContractDocumentReference               : TArray<TContractDocumentReferenceType>   read FContractDocumentReference                write FContractDocumentReference;
      [NameSpace('cac')]
      property AdditionalDocumentReference             : TArray<TAdditionalDocumentReferenceType> read FAdditionalDocumentReference              write FAdditionalDocumentReference;
      [NameSpace('cac')][Zorunlu]
      property Signature                               : TArray<TSignatureType>                   read FSignature                                write FSignature;
      [NameSpace('cac')][Zorunlu]
      property AccountingSupplierParty                 : TAccountingSupplierPartyType             read FAccountingSupplierParty                  write FAccountingSupplierParty;
      [NameSpace('cac')][Zorunlu]
      property AccountingCustomerParty                 : TAccountingCustomerPartyType             read FAccountingCustomerParty                  write FAccountingCustomerParty;
      [NameSpace('cac')]
      property BuyerCustomerParty                      : TBuyerCustomerPartyType                  read FBuyerCustomerParty                       write FBuyerCustomerParty;
      [NameSpace('cac')]
      property SellerSupplierParty                     : TSellerSupplierPartyType                 read FSellerSupplierParty                      write FSellerSupplierParty;
      [NameSpace('cac')]
      property TaxRepresentativeParty                  : TTaxRepresentativePartyType              read FTaxRepresentativeParty                   write FTaxRepresentativeParty;
      [NameSpace('cac')]
      property Delivery                                : TArray<TDeliveryType>                    read FDelivery                                 write FDelivery;
      [NameSpace('cac')]
      property PaymentMeans                            : TArray<TPaymentMeansType>                read FPaymentMeans                             write FPaymentMeans;
      [NameSpace('cac')]
      property PaymentTerms                            : TPaymentTermsType                        read FPaymentTerms                             write FPaymentTerms;
      [NameSpace('cac')]
      property AllowanceCharge                         : TArray<TAllowanceChargeType>             read FAllowanceCharge                          write FAllowanceCharge;
      [NameSpace('cac')]
      property TaxExchangeRate                         : TTaxExchangeRateType                     read FTaxExchangeRate                          write FTaxExchangeRate;
      [NameSpace('cac')]
      property PricingExchangeRate                     : TPricingExchangeRateType                 read FPricingExchangeRate                      write FPricingExchangeRate;
      [NameSpace('cac')]
      property PaymentExchangeRate                     : TPaymentExchangeRateType                 read FPaymentExchangeRate                      write FPaymentExchangeRate;
      [NameSpace('cac')]
      property PaymentAlternativeExchangeRate          : TPaymentAlternativeExchangeRateType      read FPaymentAlternativeExchangeRate           write FPaymentAlternativeExchangeRate;
      [NameSpace('cac')][Zorunlu]
      property TaxTotal                                : TArray<TTaxTotalType>                    read FTaxTotal                                 write FTaxTotal;
      [NameSpace('cac')]
      property WithholdingTaxTotal                     : TArray<TWithholdingTaxTotalType>         read FWithholdingTaxTotal                      write FWithholdingTaxTotal;
      [NameSpace('cac')][Zorunlu]
      property LegalMonetaryTotal                      : TLegalMonetaryTotalType                  read FLegalMonetaryTotal                       write FLegalMonetaryTotal;
      [NameSpace('cac')][Zorunlu]
      property InvoiceLine                             : TArray<TInvoiceLineType>                 read FInvoiceLine                              write FInvoiceLine;
  end;

implementation

end.
