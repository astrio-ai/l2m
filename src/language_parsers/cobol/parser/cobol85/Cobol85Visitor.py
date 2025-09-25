# Generated from Cobol85.g4 by ANTLR 4.13.2
from antlr4 import *
if "." in __name__:
    from .Cobol85Parser import Cobol85Parser
else:
    from Cobol85Parser import Cobol85Parser

# This class defines a complete generic visitor for a parse tree produced by Cobol85Parser.

class Cobol85Visitor(ParseTreeVisitor):

    # Visit a parse tree produced by Cobol85Parser#startRule.
    def visitStartRule(self, ctx:Cobol85Parser.StartRuleContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#compilationUnit.
    def visitCompilationUnit(self, ctx:Cobol85Parser.CompilationUnitContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#programUnit.
    def visitProgramUnit(self, ctx:Cobol85Parser.ProgramUnitContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#endProgramStatement.
    def visitEndProgramStatement(self, ctx:Cobol85Parser.EndProgramStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#identificationDivision.
    def visitIdentificationDivision(self, ctx:Cobol85Parser.IdentificationDivisionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#identificationDivisionBody.
    def visitIdentificationDivisionBody(self, ctx:Cobol85Parser.IdentificationDivisionBodyContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#programIdParagraph.
    def visitProgramIdParagraph(self, ctx:Cobol85Parser.ProgramIdParagraphContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#authorParagraph.
    def visitAuthorParagraph(self, ctx:Cobol85Parser.AuthorParagraphContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#installationParagraph.
    def visitInstallationParagraph(self, ctx:Cobol85Parser.InstallationParagraphContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#dateWrittenParagraph.
    def visitDateWrittenParagraph(self, ctx:Cobol85Parser.DateWrittenParagraphContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#dateCompiledParagraph.
    def visitDateCompiledParagraph(self, ctx:Cobol85Parser.DateCompiledParagraphContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#securityParagraph.
    def visitSecurityParagraph(self, ctx:Cobol85Parser.SecurityParagraphContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#remarksParagraph.
    def visitRemarksParagraph(self, ctx:Cobol85Parser.RemarksParagraphContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#environmentDivision.
    def visitEnvironmentDivision(self, ctx:Cobol85Parser.EnvironmentDivisionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#environmentDivisionBody.
    def visitEnvironmentDivisionBody(self, ctx:Cobol85Parser.EnvironmentDivisionBodyContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#configurationSection.
    def visitConfigurationSection(self, ctx:Cobol85Parser.ConfigurationSectionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#configurationSectionParagraph.
    def visitConfigurationSectionParagraph(self, ctx:Cobol85Parser.ConfigurationSectionParagraphContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#sourceComputerParagraph.
    def visitSourceComputerParagraph(self, ctx:Cobol85Parser.SourceComputerParagraphContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#objectComputerParagraph.
    def visitObjectComputerParagraph(self, ctx:Cobol85Parser.ObjectComputerParagraphContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#objectComputerClause.
    def visitObjectComputerClause(self, ctx:Cobol85Parser.ObjectComputerClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#memorySizeClause.
    def visitMemorySizeClause(self, ctx:Cobol85Parser.MemorySizeClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#diskSizeClause.
    def visitDiskSizeClause(self, ctx:Cobol85Parser.DiskSizeClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#collatingSequenceClause.
    def visitCollatingSequenceClause(self, ctx:Cobol85Parser.CollatingSequenceClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#collatingSequenceClauseAlphanumeric.
    def visitCollatingSequenceClauseAlphanumeric(self, ctx:Cobol85Parser.CollatingSequenceClauseAlphanumericContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#collatingSequenceClauseNational.
    def visitCollatingSequenceClauseNational(self, ctx:Cobol85Parser.CollatingSequenceClauseNationalContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#segmentLimitClause.
    def visitSegmentLimitClause(self, ctx:Cobol85Parser.SegmentLimitClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#characterSetClause.
    def visitCharacterSetClause(self, ctx:Cobol85Parser.CharacterSetClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#specialNamesParagraph.
    def visitSpecialNamesParagraph(self, ctx:Cobol85Parser.SpecialNamesParagraphContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#specialNameClause.
    def visitSpecialNameClause(self, ctx:Cobol85Parser.SpecialNameClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#alphabetClause.
    def visitAlphabetClause(self, ctx:Cobol85Parser.AlphabetClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#alphabetClauseFormat1.
    def visitAlphabetClauseFormat1(self, ctx:Cobol85Parser.AlphabetClauseFormat1Context):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#alphabetLiterals.
    def visitAlphabetLiterals(self, ctx:Cobol85Parser.AlphabetLiteralsContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#alphabetThrough.
    def visitAlphabetThrough(self, ctx:Cobol85Parser.AlphabetThroughContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#alphabetAlso.
    def visitAlphabetAlso(self, ctx:Cobol85Parser.AlphabetAlsoContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#alphabetClauseFormat2.
    def visitAlphabetClauseFormat2(self, ctx:Cobol85Parser.AlphabetClauseFormat2Context):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#channelClause.
    def visitChannelClause(self, ctx:Cobol85Parser.ChannelClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#classClause.
    def visitClassClause(self, ctx:Cobol85Parser.ClassClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#classClauseThrough.
    def visitClassClauseThrough(self, ctx:Cobol85Parser.ClassClauseThroughContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#classClauseFrom.
    def visitClassClauseFrom(self, ctx:Cobol85Parser.ClassClauseFromContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#classClauseTo.
    def visitClassClauseTo(self, ctx:Cobol85Parser.ClassClauseToContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#currencySignClause.
    def visitCurrencySignClause(self, ctx:Cobol85Parser.CurrencySignClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#decimalPointClause.
    def visitDecimalPointClause(self, ctx:Cobol85Parser.DecimalPointClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#defaultComputationalSignClause.
    def visitDefaultComputationalSignClause(self, ctx:Cobol85Parser.DefaultComputationalSignClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#defaultDisplaySignClause.
    def visitDefaultDisplaySignClause(self, ctx:Cobol85Parser.DefaultDisplaySignClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#environmentSwitchNameClause.
    def visitEnvironmentSwitchNameClause(self, ctx:Cobol85Parser.EnvironmentSwitchNameClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#environmentSwitchNameSpecialNamesStatusPhrase.
    def visitEnvironmentSwitchNameSpecialNamesStatusPhrase(self, ctx:Cobol85Parser.EnvironmentSwitchNameSpecialNamesStatusPhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#odtClause.
    def visitOdtClause(self, ctx:Cobol85Parser.OdtClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#reserveNetworkClause.
    def visitReserveNetworkClause(self, ctx:Cobol85Parser.ReserveNetworkClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#symbolicCharactersClause.
    def visitSymbolicCharactersClause(self, ctx:Cobol85Parser.SymbolicCharactersClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#symbolicCharacters.
    def visitSymbolicCharacters(self, ctx:Cobol85Parser.SymbolicCharactersContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#inputOutputSection.
    def visitInputOutputSection(self, ctx:Cobol85Parser.InputOutputSectionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#inputOutputSectionParagraph.
    def visitInputOutputSectionParagraph(self, ctx:Cobol85Parser.InputOutputSectionParagraphContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#fileControlParagraph.
    def visitFileControlParagraph(self, ctx:Cobol85Parser.FileControlParagraphContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#fileControlEntry.
    def visitFileControlEntry(self, ctx:Cobol85Parser.FileControlEntryContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#selectClause.
    def visitSelectClause(self, ctx:Cobol85Parser.SelectClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#fileControlClause.
    def visitFileControlClause(self, ctx:Cobol85Parser.FileControlClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#assignClause.
    def visitAssignClause(self, ctx:Cobol85Parser.AssignClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#reserveClause.
    def visitReserveClause(self, ctx:Cobol85Parser.ReserveClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#organizationClause.
    def visitOrganizationClause(self, ctx:Cobol85Parser.OrganizationClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#paddingCharacterClause.
    def visitPaddingCharacterClause(self, ctx:Cobol85Parser.PaddingCharacterClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#recordDelimiterClause.
    def visitRecordDelimiterClause(self, ctx:Cobol85Parser.RecordDelimiterClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#accessModeClause.
    def visitAccessModeClause(self, ctx:Cobol85Parser.AccessModeClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#recordKeyClause.
    def visitRecordKeyClause(self, ctx:Cobol85Parser.RecordKeyClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#alternateRecordKeyClause.
    def visitAlternateRecordKeyClause(self, ctx:Cobol85Parser.AlternateRecordKeyClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#passwordClause.
    def visitPasswordClause(self, ctx:Cobol85Parser.PasswordClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#fileStatusClause.
    def visitFileStatusClause(self, ctx:Cobol85Parser.FileStatusClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#relativeKeyClause.
    def visitRelativeKeyClause(self, ctx:Cobol85Parser.RelativeKeyClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#ioControlParagraph.
    def visitIoControlParagraph(self, ctx:Cobol85Parser.IoControlParagraphContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#ioControlClause.
    def visitIoControlClause(self, ctx:Cobol85Parser.IoControlClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#rerunClause.
    def visitRerunClause(self, ctx:Cobol85Parser.RerunClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#rerunEveryRecords.
    def visitRerunEveryRecords(self, ctx:Cobol85Parser.RerunEveryRecordsContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#rerunEveryOf.
    def visitRerunEveryOf(self, ctx:Cobol85Parser.RerunEveryOfContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#rerunEveryClock.
    def visitRerunEveryClock(self, ctx:Cobol85Parser.RerunEveryClockContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#sameClause.
    def visitSameClause(self, ctx:Cobol85Parser.SameClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#multipleFileClause.
    def visitMultipleFileClause(self, ctx:Cobol85Parser.MultipleFileClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#multipleFilePosition.
    def visitMultipleFilePosition(self, ctx:Cobol85Parser.MultipleFilePositionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#commitmentControlClause.
    def visitCommitmentControlClause(self, ctx:Cobol85Parser.CommitmentControlClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#dataDivision.
    def visitDataDivision(self, ctx:Cobol85Parser.DataDivisionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#dataDivisionSection.
    def visitDataDivisionSection(self, ctx:Cobol85Parser.DataDivisionSectionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#fileSection.
    def visitFileSection(self, ctx:Cobol85Parser.FileSectionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#fileDescriptionEntry.
    def visitFileDescriptionEntry(self, ctx:Cobol85Parser.FileDescriptionEntryContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#fileDescriptionEntryClause.
    def visitFileDescriptionEntryClause(self, ctx:Cobol85Parser.FileDescriptionEntryClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#externalClause.
    def visitExternalClause(self, ctx:Cobol85Parser.ExternalClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#globalClause.
    def visitGlobalClause(self, ctx:Cobol85Parser.GlobalClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#blockContainsClause.
    def visitBlockContainsClause(self, ctx:Cobol85Parser.BlockContainsClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#blockContainsTo.
    def visitBlockContainsTo(self, ctx:Cobol85Parser.BlockContainsToContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#recordContainsClause.
    def visitRecordContainsClause(self, ctx:Cobol85Parser.RecordContainsClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#recordContainsClauseFormat1.
    def visitRecordContainsClauseFormat1(self, ctx:Cobol85Parser.RecordContainsClauseFormat1Context):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#recordContainsClauseFormat2.
    def visitRecordContainsClauseFormat2(self, ctx:Cobol85Parser.RecordContainsClauseFormat2Context):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#recordContainsClauseFormat3.
    def visitRecordContainsClauseFormat3(self, ctx:Cobol85Parser.RecordContainsClauseFormat3Context):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#recordContainsTo.
    def visitRecordContainsTo(self, ctx:Cobol85Parser.RecordContainsToContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#labelRecordsClause.
    def visitLabelRecordsClause(self, ctx:Cobol85Parser.LabelRecordsClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#valueOfClause.
    def visitValueOfClause(self, ctx:Cobol85Parser.ValueOfClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#valuePair.
    def visitValuePair(self, ctx:Cobol85Parser.ValuePairContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#dataRecordsClause.
    def visitDataRecordsClause(self, ctx:Cobol85Parser.DataRecordsClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#linageClause.
    def visitLinageClause(self, ctx:Cobol85Parser.LinageClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#linageAt.
    def visitLinageAt(self, ctx:Cobol85Parser.LinageAtContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#linageFootingAt.
    def visitLinageFootingAt(self, ctx:Cobol85Parser.LinageFootingAtContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#linageLinesAtTop.
    def visitLinageLinesAtTop(self, ctx:Cobol85Parser.LinageLinesAtTopContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#linageLinesAtBottom.
    def visitLinageLinesAtBottom(self, ctx:Cobol85Parser.LinageLinesAtBottomContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#recordingModeClause.
    def visitRecordingModeClause(self, ctx:Cobol85Parser.RecordingModeClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#modeStatement.
    def visitModeStatement(self, ctx:Cobol85Parser.ModeStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#codeSetClause.
    def visitCodeSetClause(self, ctx:Cobol85Parser.CodeSetClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#reportClause.
    def visitReportClause(self, ctx:Cobol85Parser.ReportClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#dataBaseSection.
    def visitDataBaseSection(self, ctx:Cobol85Parser.DataBaseSectionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#dataBaseSectionEntry.
    def visitDataBaseSectionEntry(self, ctx:Cobol85Parser.DataBaseSectionEntryContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#workingStorageSection.
    def visitWorkingStorageSection(self, ctx:Cobol85Parser.WorkingStorageSectionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#linkageSection.
    def visitLinkageSection(self, ctx:Cobol85Parser.LinkageSectionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#communicationSection.
    def visitCommunicationSection(self, ctx:Cobol85Parser.CommunicationSectionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#communicationDescriptionEntry.
    def visitCommunicationDescriptionEntry(self, ctx:Cobol85Parser.CommunicationDescriptionEntryContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#communicationDescriptionEntryFormat1.
    def visitCommunicationDescriptionEntryFormat1(self, ctx:Cobol85Parser.CommunicationDescriptionEntryFormat1Context):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#communicationDescriptionEntryFormat2.
    def visitCommunicationDescriptionEntryFormat2(self, ctx:Cobol85Parser.CommunicationDescriptionEntryFormat2Context):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#communicationDescriptionEntryFormat3.
    def visitCommunicationDescriptionEntryFormat3(self, ctx:Cobol85Parser.CommunicationDescriptionEntryFormat3Context):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#destinationCountClause.
    def visitDestinationCountClause(self, ctx:Cobol85Parser.DestinationCountClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#destinationTableClause.
    def visitDestinationTableClause(self, ctx:Cobol85Parser.DestinationTableClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#endKeyClause.
    def visitEndKeyClause(self, ctx:Cobol85Parser.EndKeyClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#errorKeyClause.
    def visitErrorKeyClause(self, ctx:Cobol85Parser.ErrorKeyClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#messageCountClause.
    def visitMessageCountClause(self, ctx:Cobol85Parser.MessageCountClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#messageDateClause.
    def visitMessageDateClause(self, ctx:Cobol85Parser.MessageDateClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#messageTimeClause.
    def visitMessageTimeClause(self, ctx:Cobol85Parser.MessageTimeClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#statusKeyClause.
    def visitStatusKeyClause(self, ctx:Cobol85Parser.StatusKeyClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#symbolicDestinationClause.
    def visitSymbolicDestinationClause(self, ctx:Cobol85Parser.SymbolicDestinationClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#symbolicQueueClause.
    def visitSymbolicQueueClause(self, ctx:Cobol85Parser.SymbolicQueueClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#symbolicSourceClause.
    def visitSymbolicSourceClause(self, ctx:Cobol85Parser.SymbolicSourceClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#symbolicTerminalClause.
    def visitSymbolicTerminalClause(self, ctx:Cobol85Parser.SymbolicTerminalClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#symbolicSubQueueClause.
    def visitSymbolicSubQueueClause(self, ctx:Cobol85Parser.SymbolicSubQueueClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#textLengthClause.
    def visitTextLengthClause(self, ctx:Cobol85Parser.TextLengthClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#localStorageSection.
    def visitLocalStorageSection(self, ctx:Cobol85Parser.LocalStorageSectionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#screenSection.
    def visitScreenSection(self, ctx:Cobol85Parser.ScreenSectionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#screenDescriptionEntry.
    def visitScreenDescriptionEntry(self, ctx:Cobol85Parser.ScreenDescriptionEntryContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#screenDescriptionBlankClause.
    def visitScreenDescriptionBlankClause(self, ctx:Cobol85Parser.ScreenDescriptionBlankClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#screenDescriptionBellClause.
    def visitScreenDescriptionBellClause(self, ctx:Cobol85Parser.ScreenDescriptionBellClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#screenDescriptionBlinkClause.
    def visitScreenDescriptionBlinkClause(self, ctx:Cobol85Parser.ScreenDescriptionBlinkClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#screenDescriptionEraseClause.
    def visitScreenDescriptionEraseClause(self, ctx:Cobol85Parser.ScreenDescriptionEraseClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#screenDescriptionLightClause.
    def visitScreenDescriptionLightClause(self, ctx:Cobol85Parser.ScreenDescriptionLightClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#screenDescriptionGridClause.
    def visitScreenDescriptionGridClause(self, ctx:Cobol85Parser.ScreenDescriptionGridClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#screenDescriptionReverseVideoClause.
    def visitScreenDescriptionReverseVideoClause(self, ctx:Cobol85Parser.ScreenDescriptionReverseVideoClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#screenDescriptionUnderlineClause.
    def visitScreenDescriptionUnderlineClause(self, ctx:Cobol85Parser.ScreenDescriptionUnderlineClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#screenDescriptionSizeClause.
    def visitScreenDescriptionSizeClause(self, ctx:Cobol85Parser.ScreenDescriptionSizeClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#screenDescriptionLineClause.
    def visitScreenDescriptionLineClause(self, ctx:Cobol85Parser.ScreenDescriptionLineClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#screenDescriptionColumnClause.
    def visitScreenDescriptionColumnClause(self, ctx:Cobol85Parser.ScreenDescriptionColumnClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#screenDescriptionForegroundColorClause.
    def visitScreenDescriptionForegroundColorClause(self, ctx:Cobol85Parser.ScreenDescriptionForegroundColorClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#screenDescriptionBackgroundColorClause.
    def visitScreenDescriptionBackgroundColorClause(self, ctx:Cobol85Parser.ScreenDescriptionBackgroundColorClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#screenDescriptionControlClause.
    def visitScreenDescriptionControlClause(self, ctx:Cobol85Parser.ScreenDescriptionControlClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#screenDescriptionValueClause.
    def visitScreenDescriptionValueClause(self, ctx:Cobol85Parser.ScreenDescriptionValueClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#screenDescriptionPictureClause.
    def visitScreenDescriptionPictureClause(self, ctx:Cobol85Parser.ScreenDescriptionPictureClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#screenDescriptionFromClause.
    def visitScreenDescriptionFromClause(self, ctx:Cobol85Parser.ScreenDescriptionFromClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#screenDescriptionToClause.
    def visitScreenDescriptionToClause(self, ctx:Cobol85Parser.ScreenDescriptionToClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#screenDescriptionUsingClause.
    def visitScreenDescriptionUsingClause(self, ctx:Cobol85Parser.ScreenDescriptionUsingClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#screenDescriptionUsageClause.
    def visitScreenDescriptionUsageClause(self, ctx:Cobol85Parser.ScreenDescriptionUsageClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#screenDescriptionBlankWhenZeroClause.
    def visitScreenDescriptionBlankWhenZeroClause(self, ctx:Cobol85Parser.ScreenDescriptionBlankWhenZeroClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#screenDescriptionJustifiedClause.
    def visitScreenDescriptionJustifiedClause(self, ctx:Cobol85Parser.ScreenDescriptionJustifiedClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#screenDescriptionSignClause.
    def visitScreenDescriptionSignClause(self, ctx:Cobol85Parser.ScreenDescriptionSignClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#screenDescriptionAutoClause.
    def visitScreenDescriptionAutoClause(self, ctx:Cobol85Parser.ScreenDescriptionAutoClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#screenDescriptionSecureClause.
    def visitScreenDescriptionSecureClause(self, ctx:Cobol85Parser.ScreenDescriptionSecureClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#screenDescriptionRequiredClause.
    def visitScreenDescriptionRequiredClause(self, ctx:Cobol85Parser.ScreenDescriptionRequiredClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#screenDescriptionPromptClause.
    def visitScreenDescriptionPromptClause(self, ctx:Cobol85Parser.ScreenDescriptionPromptClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#screenDescriptionPromptOccursClause.
    def visitScreenDescriptionPromptOccursClause(self, ctx:Cobol85Parser.ScreenDescriptionPromptOccursClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#screenDescriptionFullClause.
    def visitScreenDescriptionFullClause(self, ctx:Cobol85Parser.ScreenDescriptionFullClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#screenDescriptionZeroFillClause.
    def visitScreenDescriptionZeroFillClause(self, ctx:Cobol85Parser.ScreenDescriptionZeroFillClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#reportSection.
    def visitReportSection(self, ctx:Cobol85Parser.ReportSectionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#reportDescription.
    def visitReportDescription(self, ctx:Cobol85Parser.ReportDescriptionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#reportDescriptionEntry.
    def visitReportDescriptionEntry(self, ctx:Cobol85Parser.ReportDescriptionEntryContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#reportDescriptionGlobalClause.
    def visitReportDescriptionGlobalClause(self, ctx:Cobol85Parser.ReportDescriptionGlobalClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#reportDescriptionPageLimitClause.
    def visitReportDescriptionPageLimitClause(self, ctx:Cobol85Parser.ReportDescriptionPageLimitClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#reportDescriptionHeadingClause.
    def visitReportDescriptionHeadingClause(self, ctx:Cobol85Parser.ReportDescriptionHeadingClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#reportDescriptionFirstDetailClause.
    def visitReportDescriptionFirstDetailClause(self, ctx:Cobol85Parser.ReportDescriptionFirstDetailClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#reportDescriptionLastDetailClause.
    def visitReportDescriptionLastDetailClause(self, ctx:Cobol85Parser.ReportDescriptionLastDetailClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#reportDescriptionFootingClause.
    def visitReportDescriptionFootingClause(self, ctx:Cobol85Parser.ReportDescriptionFootingClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#reportGroupDescriptionEntry.
    def visitReportGroupDescriptionEntry(self, ctx:Cobol85Parser.ReportGroupDescriptionEntryContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#reportGroupDescriptionEntryFormat1.
    def visitReportGroupDescriptionEntryFormat1(self, ctx:Cobol85Parser.ReportGroupDescriptionEntryFormat1Context):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#reportGroupDescriptionEntryFormat2.
    def visitReportGroupDescriptionEntryFormat2(self, ctx:Cobol85Parser.ReportGroupDescriptionEntryFormat2Context):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#reportGroupDescriptionEntryFormat3.
    def visitReportGroupDescriptionEntryFormat3(self, ctx:Cobol85Parser.ReportGroupDescriptionEntryFormat3Context):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#reportGroupBlankWhenZeroClause.
    def visitReportGroupBlankWhenZeroClause(self, ctx:Cobol85Parser.ReportGroupBlankWhenZeroClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#reportGroupColumnNumberClause.
    def visitReportGroupColumnNumberClause(self, ctx:Cobol85Parser.ReportGroupColumnNumberClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#reportGroupIndicateClause.
    def visitReportGroupIndicateClause(self, ctx:Cobol85Parser.ReportGroupIndicateClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#reportGroupJustifiedClause.
    def visitReportGroupJustifiedClause(self, ctx:Cobol85Parser.ReportGroupJustifiedClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#reportGroupLineNumberClause.
    def visitReportGroupLineNumberClause(self, ctx:Cobol85Parser.ReportGroupLineNumberClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#reportGroupLineNumberNextPage.
    def visitReportGroupLineNumberNextPage(self, ctx:Cobol85Parser.ReportGroupLineNumberNextPageContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#reportGroupLineNumberPlus.
    def visitReportGroupLineNumberPlus(self, ctx:Cobol85Parser.ReportGroupLineNumberPlusContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#reportGroupNextGroupClause.
    def visitReportGroupNextGroupClause(self, ctx:Cobol85Parser.ReportGroupNextGroupClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#reportGroupNextGroupPlus.
    def visitReportGroupNextGroupPlus(self, ctx:Cobol85Parser.ReportGroupNextGroupPlusContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#reportGroupNextGroupNextPage.
    def visitReportGroupNextGroupNextPage(self, ctx:Cobol85Parser.ReportGroupNextGroupNextPageContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#reportGroupPictureClause.
    def visitReportGroupPictureClause(self, ctx:Cobol85Parser.ReportGroupPictureClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#reportGroupResetClause.
    def visitReportGroupResetClause(self, ctx:Cobol85Parser.ReportGroupResetClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#reportGroupSignClause.
    def visitReportGroupSignClause(self, ctx:Cobol85Parser.ReportGroupSignClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#reportGroupSourceClause.
    def visitReportGroupSourceClause(self, ctx:Cobol85Parser.ReportGroupSourceClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#reportGroupSumClause.
    def visitReportGroupSumClause(self, ctx:Cobol85Parser.ReportGroupSumClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#reportGroupTypeClause.
    def visitReportGroupTypeClause(self, ctx:Cobol85Parser.ReportGroupTypeClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#reportGroupTypeReportHeading.
    def visitReportGroupTypeReportHeading(self, ctx:Cobol85Parser.ReportGroupTypeReportHeadingContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#reportGroupTypePageHeading.
    def visitReportGroupTypePageHeading(self, ctx:Cobol85Parser.ReportGroupTypePageHeadingContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#reportGroupTypeControlHeading.
    def visitReportGroupTypeControlHeading(self, ctx:Cobol85Parser.ReportGroupTypeControlHeadingContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#reportGroupTypeDetail.
    def visitReportGroupTypeDetail(self, ctx:Cobol85Parser.ReportGroupTypeDetailContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#reportGroupTypeControlFooting.
    def visitReportGroupTypeControlFooting(self, ctx:Cobol85Parser.ReportGroupTypeControlFootingContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#reportGroupUsageClause.
    def visitReportGroupUsageClause(self, ctx:Cobol85Parser.ReportGroupUsageClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#reportGroupTypePageFooting.
    def visitReportGroupTypePageFooting(self, ctx:Cobol85Parser.ReportGroupTypePageFootingContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#reportGroupTypeReportFooting.
    def visitReportGroupTypeReportFooting(self, ctx:Cobol85Parser.ReportGroupTypeReportFootingContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#reportGroupValueClause.
    def visitReportGroupValueClause(self, ctx:Cobol85Parser.ReportGroupValueClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#programLibrarySection.
    def visitProgramLibrarySection(self, ctx:Cobol85Parser.ProgramLibrarySectionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#libraryDescriptionEntry.
    def visitLibraryDescriptionEntry(self, ctx:Cobol85Parser.LibraryDescriptionEntryContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#libraryDescriptionEntryFormat1.
    def visitLibraryDescriptionEntryFormat1(self, ctx:Cobol85Parser.LibraryDescriptionEntryFormat1Context):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#libraryDescriptionEntryFormat2.
    def visitLibraryDescriptionEntryFormat2(self, ctx:Cobol85Parser.LibraryDescriptionEntryFormat2Context):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#libraryAttributeClauseFormat1.
    def visitLibraryAttributeClauseFormat1(self, ctx:Cobol85Parser.LibraryAttributeClauseFormat1Context):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#libraryAttributeClauseFormat2.
    def visitLibraryAttributeClauseFormat2(self, ctx:Cobol85Parser.LibraryAttributeClauseFormat2Context):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#libraryAttributeFunction.
    def visitLibraryAttributeFunction(self, ctx:Cobol85Parser.LibraryAttributeFunctionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#libraryAttributeParameter.
    def visitLibraryAttributeParameter(self, ctx:Cobol85Parser.LibraryAttributeParameterContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#libraryAttributeTitle.
    def visitLibraryAttributeTitle(self, ctx:Cobol85Parser.LibraryAttributeTitleContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#libraryEntryProcedureClauseFormat1.
    def visitLibraryEntryProcedureClauseFormat1(self, ctx:Cobol85Parser.LibraryEntryProcedureClauseFormat1Context):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#libraryEntryProcedureClauseFormat2.
    def visitLibraryEntryProcedureClauseFormat2(self, ctx:Cobol85Parser.LibraryEntryProcedureClauseFormat2Context):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#libraryEntryProcedureForClause.
    def visitLibraryEntryProcedureForClause(self, ctx:Cobol85Parser.LibraryEntryProcedureForClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#libraryEntryProcedureGivingClause.
    def visitLibraryEntryProcedureGivingClause(self, ctx:Cobol85Parser.LibraryEntryProcedureGivingClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#libraryEntryProcedureUsingClause.
    def visitLibraryEntryProcedureUsingClause(self, ctx:Cobol85Parser.LibraryEntryProcedureUsingClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#libraryEntryProcedureUsingName.
    def visitLibraryEntryProcedureUsingName(self, ctx:Cobol85Parser.LibraryEntryProcedureUsingNameContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#libraryEntryProcedureWithClause.
    def visitLibraryEntryProcedureWithClause(self, ctx:Cobol85Parser.LibraryEntryProcedureWithClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#libraryEntryProcedureWithName.
    def visitLibraryEntryProcedureWithName(self, ctx:Cobol85Parser.LibraryEntryProcedureWithNameContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#libraryIsCommonClause.
    def visitLibraryIsCommonClause(self, ctx:Cobol85Parser.LibraryIsCommonClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#libraryIsGlobalClause.
    def visitLibraryIsGlobalClause(self, ctx:Cobol85Parser.LibraryIsGlobalClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#dataDescriptionEntry.
    def visitDataDescriptionEntry(self, ctx:Cobol85Parser.DataDescriptionEntryContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#dataDescriptionEntryFormat1.
    def visitDataDescriptionEntryFormat1(self, ctx:Cobol85Parser.DataDescriptionEntryFormat1Context):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#dataDescriptionEntryFormat2.
    def visitDataDescriptionEntryFormat2(self, ctx:Cobol85Parser.DataDescriptionEntryFormat2Context):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#dataDescriptionEntryFormat3.
    def visitDataDescriptionEntryFormat3(self, ctx:Cobol85Parser.DataDescriptionEntryFormat3Context):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#dataDescriptionEntryExecSql.
    def visitDataDescriptionEntryExecSql(self, ctx:Cobol85Parser.DataDescriptionEntryExecSqlContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#dataAlignedClause.
    def visitDataAlignedClause(self, ctx:Cobol85Parser.DataAlignedClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#dataBlankWhenZeroClause.
    def visitDataBlankWhenZeroClause(self, ctx:Cobol85Parser.DataBlankWhenZeroClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#dataCommonOwnLocalClause.
    def visitDataCommonOwnLocalClause(self, ctx:Cobol85Parser.DataCommonOwnLocalClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#dataExternalClause.
    def visitDataExternalClause(self, ctx:Cobol85Parser.DataExternalClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#dataGlobalClause.
    def visitDataGlobalClause(self, ctx:Cobol85Parser.DataGlobalClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#dataIntegerStringClause.
    def visitDataIntegerStringClause(self, ctx:Cobol85Parser.DataIntegerStringClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#dataJustifiedClause.
    def visitDataJustifiedClause(self, ctx:Cobol85Parser.DataJustifiedClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#dataOccursClause.
    def visitDataOccursClause(self, ctx:Cobol85Parser.DataOccursClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#dataOccursTo.
    def visitDataOccursTo(self, ctx:Cobol85Parser.DataOccursToContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#dataOccursSort.
    def visitDataOccursSort(self, ctx:Cobol85Parser.DataOccursSortContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#dataPictureClause.
    def visitDataPictureClause(self, ctx:Cobol85Parser.DataPictureClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#pictureString.
    def visitPictureString(self, ctx:Cobol85Parser.PictureStringContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#pictureChars.
    def visitPictureChars(self, ctx:Cobol85Parser.PictureCharsContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#pictureCardinality.
    def visitPictureCardinality(self, ctx:Cobol85Parser.PictureCardinalityContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#dataReceivedByClause.
    def visitDataReceivedByClause(self, ctx:Cobol85Parser.DataReceivedByClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#dataRecordAreaClause.
    def visitDataRecordAreaClause(self, ctx:Cobol85Parser.DataRecordAreaClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#dataRedefinesClause.
    def visitDataRedefinesClause(self, ctx:Cobol85Parser.DataRedefinesClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#dataRenamesClause.
    def visitDataRenamesClause(self, ctx:Cobol85Parser.DataRenamesClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#dataSignClause.
    def visitDataSignClause(self, ctx:Cobol85Parser.DataSignClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#dataSynchronizedClause.
    def visitDataSynchronizedClause(self, ctx:Cobol85Parser.DataSynchronizedClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#dataThreadLocalClause.
    def visitDataThreadLocalClause(self, ctx:Cobol85Parser.DataThreadLocalClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#dataTypeClause.
    def visitDataTypeClause(self, ctx:Cobol85Parser.DataTypeClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#dataTypeDefClause.
    def visitDataTypeDefClause(self, ctx:Cobol85Parser.DataTypeDefClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#dataUsageClause.
    def visitDataUsageClause(self, ctx:Cobol85Parser.DataUsageClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#dataUsingClause.
    def visitDataUsingClause(self, ctx:Cobol85Parser.DataUsingClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#dataValueClause.
    def visitDataValueClause(self, ctx:Cobol85Parser.DataValueClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#dataValueInterval.
    def visitDataValueInterval(self, ctx:Cobol85Parser.DataValueIntervalContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#dataValueIntervalFrom.
    def visitDataValueIntervalFrom(self, ctx:Cobol85Parser.DataValueIntervalFromContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#dataValueIntervalTo.
    def visitDataValueIntervalTo(self, ctx:Cobol85Parser.DataValueIntervalToContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#dataWithLowerBoundsClause.
    def visitDataWithLowerBoundsClause(self, ctx:Cobol85Parser.DataWithLowerBoundsClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#procedureDivision.
    def visitProcedureDivision(self, ctx:Cobol85Parser.ProcedureDivisionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#procedureDivisionUsingClause.
    def visitProcedureDivisionUsingClause(self, ctx:Cobol85Parser.ProcedureDivisionUsingClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#procedureDivisionGivingClause.
    def visitProcedureDivisionGivingClause(self, ctx:Cobol85Parser.ProcedureDivisionGivingClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#procedureDivisionUsingParameter.
    def visitProcedureDivisionUsingParameter(self, ctx:Cobol85Parser.ProcedureDivisionUsingParameterContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#procedureDivisionByReferencePhrase.
    def visitProcedureDivisionByReferencePhrase(self, ctx:Cobol85Parser.ProcedureDivisionByReferencePhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#procedureDivisionByReference.
    def visitProcedureDivisionByReference(self, ctx:Cobol85Parser.ProcedureDivisionByReferenceContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#procedureDivisionByValuePhrase.
    def visitProcedureDivisionByValuePhrase(self, ctx:Cobol85Parser.ProcedureDivisionByValuePhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#procedureDivisionByValue.
    def visitProcedureDivisionByValue(self, ctx:Cobol85Parser.ProcedureDivisionByValueContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#procedureDeclaratives.
    def visitProcedureDeclaratives(self, ctx:Cobol85Parser.ProcedureDeclarativesContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#procedureDeclarative.
    def visitProcedureDeclarative(self, ctx:Cobol85Parser.ProcedureDeclarativeContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#procedureSectionHeader.
    def visitProcedureSectionHeader(self, ctx:Cobol85Parser.ProcedureSectionHeaderContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#procedureDivisionBody.
    def visitProcedureDivisionBody(self, ctx:Cobol85Parser.ProcedureDivisionBodyContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#procedureSection.
    def visitProcedureSection(self, ctx:Cobol85Parser.ProcedureSectionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#paragraphs.
    def visitParagraphs(self, ctx:Cobol85Parser.ParagraphsContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#paragraph.
    def visitParagraph(self, ctx:Cobol85Parser.ParagraphContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#sentence.
    def visitSentence(self, ctx:Cobol85Parser.SentenceContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#statement.
    def visitStatement(self, ctx:Cobol85Parser.StatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#acceptStatement.
    def visitAcceptStatement(self, ctx:Cobol85Parser.AcceptStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#acceptFromDateStatement.
    def visitAcceptFromDateStatement(self, ctx:Cobol85Parser.AcceptFromDateStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#acceptFromMnemonicStatement.
    def visitAcceptFromMnemonicStatement(self, ctx:Cobol85Parser.AcceptFromMnemonicStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#acceptFromEscapeKeyStatement.
    def visitAcceptFromEscapeKeyStatement(self, ctx:Cobol85Parser.AcceptFromEscapeKeyStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#acceptMessageCountStatement.
    def visitAcceptMessageCountStatement(self, ctx:Cobol85Parser.AcceptMessageCountStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#addStatement.
    def visitAddStatement(self, ctx:Cobol85Parser.AddStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#addToStatement.
    def visitAddToStatement(self, ctx:Cobol85Parser.AddToStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#addToGivingStatement.
    def visitAddToGivingStatement(self, ctx:Cobol85Parser.AddToGivingStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#addCorrespondingStatement.
    def visitAddCorrespondingStatement(self, ctx:Cobol85Parser.AddCorrespondingStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#addFrom.
    def visitAddFrom(self, ctx:Cobol85Parser.AddFromContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#addTo.
    def visitAddTo(self, ctx:Cobol85Parser.AddToContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#addToGiving.
    def visitAddToGiving(self, ctx:Cobol85Parser.AddToGivingContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#addGiving.
    def visitAddGiving(self, ctx:Cobol85Parser.AddGivingContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#alteredGoTo.
    def visitAlteredGoTo(self, ctx:Cobol85Parser.AlteredGoToContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#alterStatement.
    def visitAlterStatement(self, ctx:Cobol85Parser.AlterStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#alterProceedTo.
    def visitAlterProceedTo(self, ctx:Cobol85Parser.AlterProceedToContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#callStatement.
    def visitCallStatement(self, ctx:Cobol85Parser.CallStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#callUsingPhrase.
    def visitCallUsingPhrase(self, ctx:Cobol85Parser.CallUsingPhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#callUsingParameter.
    def visitCallUsingParameter(self, ctx:Cobol85Parser.CallUsingParameterContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#callByReferencePhrase.
    def visitCallByReferencePhrase(self, ctx:Cobol85Parser.CallByReferencePhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#callByReference.
    def visitCallByReference(self, ctx:Cobol85Parser.CallByReferenceContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#callByValuePhrase.
    def visitCallByValuePhrase(self, ctx:Cobol85Parser.CallByValuePhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#callByValue.
    def visitCallByValue(self, ctx:Cobol85Parser.CallByValueContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#callByContentPhrase.
    def visitCallByContentPhrase(self, ctx:Cobol85Parser.CallByContentPhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#callByContent.
    def visitCallByContent(self, ctx:Cobol85Parser.CallByContentContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#callGivingPhrase.
    def visitCallGivingPhrase(self, ctx:Cobol85Parser.CallGivingPhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#cancelStatement.
    def visitCancelStatement(self, ctx:Cobol85Parser.CancelStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#cancelCall.
    def visitCancelCall(self, ctx:Cobol85Parser.CancelCallContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#closeStatement.
    def visitCloseStatement(self, ctx:Cobol85Parser.CloseStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#closeFile.
    def visitCloseFile(self, ctx:Cobol85Parser.CloseFileContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#closeReelUnitStatement.
    def visitCloseReelUnitStatement(self, ctx:Cobol85Parser.CloseReelUnitStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#closeRelativeStatement.
    def visitCloseRelativeStatement(self, ctx:Cobol85Parser.CloseRelativeStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#closePortFileIOStatement.
    def visitClosePortFileIOStatement(self, ctx:Cobol85Parser.ClosePortFileIOStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#closePortFileIOUsing.
    def visitClosePortFileIOUsing(self, ctx:Cobol85Parser.ClosePortFileIOUsingContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#closePortFileIOUsingCloseDisposition.
    def visitClosePortFileIOUsingCloseDisposition(self, ctx:Cobol85Parser.ClosePortFileIOUsingCloseDispositionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#closePortFileIOUsingAssociatedData.
    def visitClosePortFileIOUsingAssociatedData(self, ctx:Cobol85Parser.ClosePortFileIOUsingAssociatedDataContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#closePortFileIOUsingAssociatedDataLength.
    def visitClosePortFileIOUsingAssociatedDataLength(self, ctx:Cobol85Parser.ClosePortFileIOUsingAssociatedDataLengthContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#computeStatement.
    def visitComputeStatement(self, ctx:Cobol85Parser.ComputeStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#computeStore.
    def visitComputeStore(self, ctx:Cobol85Parser.ComputeStoreContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#continueStatement.
    def visitContinueStatement(self, ctx:Cobol85Parser.ContinueStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#deleteStatement.
    def visitDeleteStatement(self, ctx:Cobol85Parser.DeleteStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#disableStatement.
    def visitDisableStatement(self, ctx:Cobol85Parser.DisableStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#displayStatement.
    def visitDisplayStatement(self, ctx:Cobol85Parser.DisplayStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#displayOperand.
    def visitDisplayOperand(self, ctx:Cobol85Parser.DisplayOperandContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#displayAt.
    def visitDisplayAt(self, ctx:Cobol85Parser.DisplayAtContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#displayUpon.
    def visitDisplayUpon(self, ctx:Cobol85Parser.DisplayUponContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#displayWith.
    def visitDisplayWith(self, ctx:Cobol85Parser.DisplayWithContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#divideStatement.
    def visitDivideStatement(self, ctx:Cobol85Parser.DivideStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#divideIntoStatement.
    def visitDivideIntoStatement(self, ctx:Cobol85Parser.DivideIntoStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#divideIntoGivingStatement.
    def visitDivideIntoGivingStatement(self, ctx:Cobol85Parser.DivideIntoGivingStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#divideByGivingStatement.
    def visitDivideByGivingStatement(self, ctx:Cobol85Parser.DivideByGivingStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#divideGivingPhrase.
    def visitDivideGivingPhrase(self, ctx:Cobol85Parser.DivideGivingPhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#divideInto.
    def visitDivideInto(self, ctx:Cobol85Parser.DivideIntoContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#divideGiving.
    def visitDivideGiving(self, ctx:Cobol85Parser.DivideGivingContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#divideRemainder.
    def visitDivideRemainder(self, ctx:Cobol85Parser.DivideRemainderContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#enableStatement.
    def visitEnableStatement(self, ctx:Cobol85Parser.EnableStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#entryStatement.
    def visitEntryStatement(self, ctx:Cobol85Parser.EntryStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#evaluateStatement.
    def visitEvaluateStatement(self, ctx:Cobol85Parser.EvaluateStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#evaluateSelect.
    def visitEvaluateSelect(self, ctx:Cobol85Parser.EvaluateSelectContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#evaluateAlsoSelect.
    def visitEvaluateAlsoSelect(self, ctx:Cobol85Parser.EvaluateAlsoSelectContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#evaluateWhenPhrase.
    def visitEvaluateWhenPhrase(self, ctx:Cobol85Parser.EvaluateWhenPhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#evaluateWhen.
    def visitEvaluateWhen(self, ctx:Cobol85Parser.EvaluateWhenContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#evaluateCondition.
    def visitEvaluateCondition(self, ctx:Cobol85Parser.EvaluateConditionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#evaluateThrough.
    def visitEvaluateThrough(self, ctx:Cobol85Parser.EvaluateThroughContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#evaluateAlsoCondition.
    def visitEvaluateAlsoCondition(self, ctx:Cobol85Parser.EvaluateAlsoConditionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#evaluateWhenOther.
    def visitEvaluateWhenOther(self, ctx:Cobol85Parser.EvaluateWhenOtherContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#evaluateValue.
    def visitEvaluateValue(self, ctx:Cobol85Parser.EvaluateValueContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#execCicsStatement.
    def visitExecCicsStatement(self, ctx:Cobol85Parser.ExecCicsStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#execSqlStatement.
    def visitExecSqlStatement(self, ctx:Cobol85Parser.ExecSqlStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#execSqlImsStatement.
    def visitExecSqlImsStatement(self, ctx:Cobol85Parser.ExecSqlImsStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#exhibitStatement.
    def visitExhibitStatement(self, ctx:Cobol85Parser.ExhibitStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#exhibitOperand.
    def visitExhibitOperand(self, ctx:Cobol85Parser.ExhibitOperandContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#exitStatement.
    def visitExitStatement(self, ctx:Cobol85Parser.ExitStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#generateStatement.
    def visitGenerateStatement(self, ctx:Cobol85Parser.GenerateStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#gobackStatement.
    def visitGobackStatement(self, ctx:Cobol85Parser.GobackStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#goToStatement.
    def visitGoToStatement(self, ctx:Cobol85Parser.GoToStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#goToStatementSimple.
    def visitGoToStatementSimple(self, ctx:Cobol85Parser.GoToStatementSimpleContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#goToDependingOnStatement.
    def visitGoToDependingOnStatement(self, ctx:Cobol85Parser.GoToDependingOnStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#ifStatement.
    def visitIfStatement(self, ctx:Cobol85Parser.IfStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#ifThen.
    def visitIfThen(self, ctx:Cobol85Parser.IfThenContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#ifElse.
    def visitIfElse(self, ctx:Cobol85Parser.IfElseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#initializeStatement.
    def visitInitializeStatement(self, ctx:Cobol85Parser.InitializeStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#initializeReplacingPhrase.
    def visitInitializeReplacingPhrase(self, ctx:Cobol85Parser.InitializeReplacingPhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#initializeReplacingBy.
    def visitInitializeReplacingBy(self, ctx:Cobol85Parser.InitializeReplacingByContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#initiateStatement.
    def visitInitiateStatement(self, ctx:Cobol85Parser.InitiateStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#inspectStatement.
    def visitInspectStatement(self, ctx:Cobol85Parser.InspectStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#inspectTallyingPhrase.
    def visitInspectTallyingPhrase(self, ctx:Cobol85Parser.InspectTallyingPhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#inspectReplacingPhrase.
    def visitInspectReplacingPhrase(self, ctx:Cobol85Parser.InspectReplacingPhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#inspectTallyingReplacingPhrase.
    def visitInspectTallyingReplacingPhrase(self, ctx:Cobol85Parser.InspectTallyingReplacingPhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#inspectConvertingPhrase.
    def visitInspectConvertingPhrase(self, ctx:Cobol85Parser.InspectConvertingPhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#inspectFor.
    def visitInspectFor(self, ctx:Cobol85Parser.InspectForContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#inspectCharacters.
    def visitInspectCharacters(self, ctx:Cobol85Parser.InspectCharactersContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#inspectReplacingCharacters.
    def visitInspectReplacingCharacters(self, ctx:Cobol85Parser.InspectReplacingCharactersContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#inspectAllLeadings.
    def visitInspectAllLeadings(self, ctx:Cobol85Parser.InspectAllLeadingsContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#inspectReplacingAllLeadings.
    def visitInspectReplacingAllLeadings(self, ctx:Cobol85Parser.InspectReplacingAllLeadingsContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#inspectAllLeading.
    def visitInspectAllLeading(self, ctx:Cobol85Parser.InspectAllLeadingContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#inspectReplacingAllLeading.
    def visitInspectReplacingAllLeading(self, ctx:Cobol85Parser.InspectReplacingAllLeadingContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#inspectBy.
    def visitInspectBy(self, ctx:Cobol85Parser.InspectByContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#inspectTo.
    def visitInspectTo(self, ctx:Cobol85Parser.InspectToContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#inspectBeforeAfter.
    def visitInspectBeforeAfter(self, ctx:Cobol85Parser.InspectBeforeAfterContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#mergeStatement.
    def visitMergeStatement(self, ctx:Cobol85Parser.MergeStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#mergeOnKeyClause.
    def visitMergeOnKeyClause(self, ctx:Cobol85Parser.MergeOnKeyClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#mergeCollatingSequencePhrase.
    def visitMergeCollatingSequencePhrase(self, ctx:Cobol85Parser.MergeCollatingSequencePhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#mergeCollatingAlphanumeric.
    def visitMergeCollatingAlphanumeric(self, ctx:Cobol85Parser.MergeCollatingAlphanumericContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#mergeCollatingNational.
    def visitMergeCollatingNational(self, ctx:Cobol85Parser.MergeCollatingNationalContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#mergeUsing.
    def visitMergeUsing(self, ctx:Cobol85Parser.MergeUsingContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#mergeOutputProcedurePhrase.
    def visitMergeOutputProcedurePhrase(self, ctx:Cobol85Parser.MergeOutputProcedurePhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#mergeOutputThrough.
    def visitMergeOutputThrough(self, ctx:Cobol85Parser.MergeOutputThroughContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#mergeGivingPhrase.
    def visitMergeGivingPhrase(self, ctx:Cobol85Parser.MergeGivingPhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#mergeGiving.
    def visitMergeGiving(self, ctx:Cobol85Parser.MergeGivingContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#moveStatement.
    def visitMoveStatement(self, ctx:Cobol85Parser.MoveStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#moveToStatement.
    def visitMoveToStatement(self, ctx:Cobol85Parser.MoveToStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#moveToSendingArea.
    def visitMoveToSendingArea(self, ctx:Cobol85Parser.MoveToSendingAreaContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#moveCorrespondingToStatement.
    def visitMoveCorrespondingToStatement(self, ctx:Cobol85Parser.MoveCorrespondingToStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#moveCorrespondingToSendingArea.
    def visitMoveCorrespondingToSendingArea(self, ctx:Cobol85Parser.MoveCorrespondingToSendingAreaContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#multiplyStatement.
    def visitMultiplyStatement(self, ctx:Cobol85Parser.MultiplyStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#multiplyRegular.
    def visitMultiplyRegular(self, ctx:Cobol85Parser.MultiplyRegularContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#multiplyRegularOperand.
    def visitMultiplyRegularOperand(self, ctx:Cobol85Parser.MultiplyRegularOperandContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#multiplyGiving.
    def visitMultiplyGiving(self, ctx:Cobol85Parser.MultiplyGivingContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#multiplyGivingOperand.
    def visitMultiplyGivingOperand(self, ctx:Cobol85Parser.MultiplyGivingOperandContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#multiplyGivingResult.
    def visitMultiplyGivingResult(self, ctx:Cobol85Parser.MultiplyGivingResultContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#openStatement.
    def visitOpenStatement(self, ctx:Cobol85Parser.OpenStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#openInputStatement.
    def visitOpenInputStatement(self, ctx:Cobol85Parser.OpenInputStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#openInput.
    def visitOpenInput(self, ctx:Cobol85Parser.OpenInputContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#openOutputStatement.
    def visitOpenOutputStatement(self, ctx:Cobol85Parser.OpenOutputStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#openOutput.
    def visitOpenOutput(self, ctx:Cobol85Parser.OpenOutputContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#openIOStatement.
    def visitOpenIOStatement(self, ctx:Cobol85Parser.OpenIOStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#openExtendStatement.
    def visitOpenExtendStatement(self, ctx:Cobol85Parser.OpenExtendStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#performStatement.
    def visitPerformStatement(self, ctx:Cobol85Parser.PerformStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#performInlineStatement.
    def visitPerformInlineStatement(self, ctx:Cobol85Parser.PerformInlineStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#performProcedureStatement.
    def visitPerformProcedureStatement(self, ctx:Cobol85Parser.PerformProcedureStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#performType.
    def visitPerformType(self, ctx:Cobol85Parser.PerformTypeContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#performTimes.
    def visitPerformTimes(self, ctx:Cobol85Parser.PerformTimesContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#performUntil.
    def visitPerformUntil(self, ctx:Cobol85Parser.PerformUntilContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#performVarying.
    def visitPerformVarying(self, ctx:Cobol85Parser.PerformVaryingContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#performVaryingClause.
    def visitPerformVaryingClause(self, ctx:Cobol85Parser.PerformVaryingClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#performVaryingPhrase.
    def visitPerformVaryingPhrase(self, ctx:Cobol85Parser.PerformVaryingPhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#performAfter.
    def visitPerformAfter(self, ctx:Cobol85Parser.PerformAfterContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#performFrom.
    def visitPerformFrom(self, ctx:Cobol85Parser.PerformFromContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#performBy.
    def visitPerformBy(self, ctx:Cobol85Parser.PerformByContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#performTestClause.
    def visitPerformTestClause(self, ctx:Cobol85Parser.PerformTestClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#purgeStatement.
    def visitPurgeStatement(self, ctx:Cobol85Parser.PurgeStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#readStatement.
    def visitReadStatement(self, ctx:Cobol85Parser.ReadStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#readInto.
    def visitReadInto(self, ctx:Cobol85Parser.ReadIntoContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#readWith.
    def visitReadWith(self, ctx:Cobol85Parser.ReadWithContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#readKey.
    def visitReadKey(self, ctx:Cobol85Parser.ReadKeyContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#receiveStatement.
    def visitReceiveStatement(self, ctx:Cobol85Parser.ReceiveStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#receiveFromStatement.
    def visitReceiveFromStatement(self, ctx:Cobol85Parser.ReceiveFromStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#receiveFrom.
    def visitReceiveFrom(self, ctx:Cobol85Parser.ReceiveFromContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#receiveIntoStatement.
    def visitReceiveIntoStatement(self, ctx:Cobol85Parser.ReceiveIntoStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#receiveNoData.
    def visitReceiveNoData(self, ctx:Cobol85Parser.ReceiveNoDataContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#receiveWithData.
    def visitReceiveWithData(self, ctx:Cobol85Parser.ReceiveWithDataContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#receiveBefore.
    def visitReceiveBefore(self, ctx:Cobol85Parser.ReceiveBeforeContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#receiveWith.
    def visitReceiveWith(self, ctx:Cobol85Parser.ReceiveWithContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#receiveThread.
    def visitReceiveThread(self, ctx:Cobol85Parser.ReceiveThreadContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#receiveSize.
    def visitReceiveSize(self, ctx:Cobol85Parser.ReceiveSizeContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#receiveStatus.
    def visitReceiveStatus(self, ctx:Cobol85Parser.ReceiveStatusContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#releaseStatement.
    def visitReleaseStatement(self, ctx:Cobol85Parser.ReleaseStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#returnStatement.
    def visitReturnStatement(self, ctx:Cobol85Parser.ReturnStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#returnInto.
    def visitReturnInto(self, ctx:Cobol85Parser.ReturnIntoContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#rewriteStatement.
    def visitRewriteStatement(self, ctx:Cobol85Parser.RewriteStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#rewriteFrom.
    def visitRewriteFrom(self, ctx:Cobol85Parser.RewriteFromContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#searchStatement.
    def visitSearchStatement(self, ctx:Cobol85Parser.SearchStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#searchVarying.
    def visitSearchVarying(self, ctx:Cobol85Parser.SearchVaryingContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#searchWhen.
    def visitSearchWhen(self, ctx:Cobol85Parser.SearchWhenContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#sendStatement.
    def visitSendStatement(self, ctx:Cobol85Parser.SendStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#sendStatementSync.
    def visitSendStatementSync(self, ctx:Cobol85Parser.SendStatementSyncContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#sendStatementAsync.
    def visitSendStatementAsync(self, ctx:Cobol85Parser.SendStatementAsyncContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#sendFromPhrase.
    def visitSendFromPhrase(self, ctx:Cobol85Parser.SendFromPhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#sendWithPhrase.
    def visitSendWithPhrase(self, ctx:Cobol85Parser.SendWithPhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#sendReplacingPhrase.
    def visitSendReplacingPhrase(self, ctx:Cobol85Parser.SendReplacingPhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#sendAdvancingPhrase.
    def visitSendAdvancingPhrase(self, ctx:Cobol85Parser.SendAdvancingPhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#sendAdvancingPage.
    def visitSendAdvancingPage(self, ctx:Cobol85Parser.SendAdvancingPageContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#sendAdvancingLines.
    def visitSendAdvancingLines(self, ctx:Cobol85Parser.SendAdvancingLinesContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#sendAdvancingMnemonic.
    def visitSendAdvancingMnemonic(self, ctx:Cobol85Parser.SendAdvancingMnemonicContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#setStatement.
    def visitSetStatement(self, ctx:Cobol85Parser.SetStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#setToStatement.
    def visitSetToStatement(self, ctx:Cobol85Parser.SetToStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#setUpDownByStatement.
    def visitSetUpDownByStatement(self, ctx:Cobol85Parser.SetUpDownByStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#setTo.
    def visitSetTo(self, ctx:Cobol85Parser.SetToContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#setToValue.
    def visitSetToValue(self, ctx:Cobol85Parser.SetToValueContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#setByValue.
    def visitSetByValue(self, ctx:Cobol85Parser.SetByValueContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#sortStatement.
    def visitSortStatement(self, ctx:Cobol85Parser.SortStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#sortOnKeyClause.
    def visitSortOnKeyClause(self, ctx:Cobol85Parser.SortOnKeyClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#sortDuplicatesPhrase.
    def visitSortDuplicatesPhrase(self, ctx:Cobol85Parser.SortDuplicatesPhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#sortCollatingSequencePhrase.
    def visitSortCollatingSequencePhrase(self, ctx:Cobol85Parser.SortCollatingSequencePhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#sortCollatingAlphanumeric.
    def visitSortCollatingAlphanumeric(self, ctx:Cobol85Parser.SortCollatingAlphanumericContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#sortCollatingNational.
    def visitSortCollatingNational(self, ctx:Cobol85Parser.SortCollatingNationalContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#sortInputProcedurePhrase.
    def visitSortInputProcedurePhrase(self, ctx:Cobol85Parser.SortInputProcedurePhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#sortInputThrough.
    def visitSortInputThrough(self, ctx:Cobol85Parser.SortInputThroughContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#sortUsing.
    def visitSortUsing(self, ctx:Cobol85Parser.SortUsingContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#sortOutputProcedurePhrase.
    def visitSortOutputProcedurePhrase(self, ctx:Cobol85Parser.SortOutputProcedurePhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#sortOutputThrough.
    def visitSortOutputThrough(self, ctx:Cobol85Parser.SortOutputThroughContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#sortGivingPhrase.
    def visitSortGivingPhrase(self, ctx:Cobol85Parser.SortGivingPhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#sortGiving.
    def visitSortGiving(self, ctx:Cobol85Parser.SortGivingContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#startStatement.
    def visitStartStatement(self, ctx:Cobol85Parser.StartStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#startKey.
    def visitStartKey(self, ctx:Cobol85Parser.StartKeyContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#stopStatement.
    def visitStopStatement(self, ctx:Cobol85Parser.StopStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#stringStatement.
    def visitStringStatement(self, ctx:Cobol85Parser.StringStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#stringSendingPhrase.
    def visitStringSendingPhrase(self, ctx:Cobol85Parser.StringSendingPhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#stringSending.
    def visitStringSending(self, ctx:Cobol85Parser.StringSendingContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#stringDelimitedByPhrase.
    def visitStringDelimitedByPhrase(self, ctx:Cobol85Parser.StringDelimitedByPhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#stringForPhrase.
    def visitStringForPhrase(self, ctx:Cobol85Parser.StringForPhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#stringIntoPhrase.
    def visitStringIntoPhrase(self, ctx:Cobol85Parser.StringIntoPhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#stringWithPointerPhrase.
    def visitStringWithPointerPhrase(self, ctx:Cobol85Parser.StringWithPointerPhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#subtractStatement.
    def visitSubtractStatement(self, ctx:Cobol85Parser.SubtractStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#subtractFromStatement.
    def visitSubtractFromStatement(self, ctx:Cobol85Parser.SubtractFromStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#subtractFromGivingStatement.
    def visitSubtractFromGivingStatement(self, ctx:Cobol85Parser.SubtractFromGivingStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#subtractCorrespondingStatement.
    def visitSubtractCorrespondingStatement(self, ctx:Cobol85Parser.SubtractCorrespondingStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#subtractSubtrahend.
    def visitSubtractSubtrahend(self, ctx:Cobol85Parser.SubtractSubtrahendContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#subtractMinuend.
    def visitSubtractMinuend(self, ctx:Cobol85Parser.SubtractMinuendContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#subtractMinuendGiving.
    def visitSubtractMinuendGiving(self, ctx:Cobol85Parser.SubtractMinuendGivingContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#subtractGiving.
    def visitSubtractGiving(self, ctx:Cobol85Parser.SubtractGivingContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#subtractMinuendCorresponding.
    def visitSubtractMinuendCorresponding(self, ctx:Cobol85Parser.SubtractMinuendCorrespondingContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#terminateStatement.
    def visitTerminateStatement(self, ctx:Cobol85Parser.TerminateStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#unstringStatement.
    def visitUnstringStatement(self, ctx:Cobol85Parser.UnstringStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#unstringSendingPhrase.
    def visitUnstringSendingPhrase(self, ctx:Cobol85Parser.UnstringSendingPhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#unstringDelimitedByPhrase.
    def visitUnstringDelimitedByPhrase(self, ctx:Cobol85Parser.UnstringDelimitedByPhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#unstringOrAllPhrase.
    def visitUnstringOrAllPhrase(self, ctx:Cobol85Parser.UnstringOrAllPhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#unstringIntoPhrase.
    def visitUnstringIntoPhrase(self, ctx:Cobol85Parser.UnstringIntoPhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#unstringInto.
    def visitUnstringInto(self, ctx:Cobol85Parser.UnstringIntoContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#unstringDelimiterIn.
    def visitUnstringDelimiterIn(self, ctx:Cobol85Parser.UnstringDelimiterInContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#unstringCountIn.
    def visitUnstringCountIn(self, ctx:Cobol85Parser.UnstringCountInContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#unstringWithPointerPhrase.
    def visitUnstringWithPointerPhrase(self, ctx:Cobol85Parser.UnstringWithPointerPhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#unstringTallyingPhrase.
    def visitUnstringTallyingPhrase(self, ctx:Cobol85Parser.UnstringTallyingPhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#useStatement.
    def visitUseStatement(self, ctx:Cobol85Parser.UseStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#useAfterClause.
    def visitUseAfterClause(self, ctx:Cobol85Parser.UseAfterClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#useAfterOn.
    def visitUseAfterOn(self, ctx:Cobol85Parser.UseAfterOnContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#useDebugClause.
    def visitUseDebugClause(self, ctx:Cobol85Parser.UseDebugClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#useDebugOn.
    def visitUseDebugOn(self, ctx:Cobol85Parser.UseDebugOnContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#writeStatement.
    def visitWriteStatement(self, ctx:Cobol85Parser.WriteStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#writeFromPhrase.
    def visitWriteFromPhrase(self, ctx:Cobol85Parser.WriteFromPhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#writeAdvancingPhrase.
    def visitWriteAdvancingPhrase(self, ctx:Cobol85Parser.WriteAdvancingPhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#writeAdvancingPage.
    def visitWriteAdvancingPage(self, ctx:Cobol85Parser.WriteAdvancingPageContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#writeAdvancingLines.
    def visitWriteAdvancingLines(self, ctx:Cobol85Parser.WriteAdvancingLinesContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#writeAdvancingMnemonic.
    def visitWriteAdvancingMnemonic(self, ctx:Cobol85Parser.WriteAdvancingMnemonicContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#writeAtEndOfPagePhrase.
    def visitWriteAtEndOfPagePhrase(self, ctx:Cobol85Parser.WriteAtEndOfPagePhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#writeNotAtEndOfPagePhrase.
    def visitWriteNotAtEndOfPagePhrase(self, ctx:Cobol85Parser.WriteNotAtEndOfPagePhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#atEndPhrase.
    def visitAtEndPhrase(self, ctx:Cobol85Parser.AtEndPhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#notAtEndPhrase.
    def visitNotAtEndPhrase(self, ctx:Cobol85Parser.NotAtEndPhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#invalidKeyPhrase.
    def visitInvalidKeyPhrase(self, ctx:Cobol85Parser.InvalidKeyPhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#notInvalidKeyPhrase.
    def visitNotInvalidKeyPhrase(self, ctx:Cobol85Parser.NotInvalidKeyPhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#onOverflowPhrase.
    def visitOnOverflowPhrase(self, ctx:Cobol85Parser.OnOverflowPhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#notOnOverflowPhrase.
    def visitNotOnOverflowPhrase(self, ctx:Cobol85Parser.NotOnOverflowPhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#onSizeErrorPhrase.
    def visitOnSizeErrorPhrase(self, ctx:Cobol85Parser.OnSizeErrorPhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#notOnSizeErrorPhrase.
    def visitNotOnSizeErrorPhrase(self, ctx:Cobol85Parser.NotOnSizeErrorPhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#onExceptionClause.
    def visitOnExceptionClause(self, ctx:Cobol85Parser.OnExceptionClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#notOnExceptionClause.
    def visitNotOnExceptionClause(self, ctx:Cobol85Parser.NotOnExceptionClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#arithmeticExpression.
    def visitArithmeticExpression(self, ctx:Cobol85Parser.ArithmeticExpressionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#plusMinus.
    def visitPlusMinus(self, ctx:Cobol85Parser.PlusMinusContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#multDivs.
    def visitMultDivs(self, ctx:Cobol85Parser.MultDivsContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#multDiv.
    def visitMultDiv(self, ctx:Cobol85Parser.MultDivContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#powers.
    def visitPowers(self, ctx:Cobol85Parser.PowersContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#power.
    def visitPower(self, ctx:Cobol85Parser.PowerContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#basis.
    def visitBasis(self, ctx:Cobol85Parser.BasisContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#condition.
    def visitCondition(self, ctx:Cobol85Parser.ConditionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#andOrCondition.
    def visitAndOrCondition(self, ctx:Cobol85Parser.AndOrConditionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#combinableCondition.
    def visitCombinableCondition(self, ctx:Cobol85Parser.CombinableConditionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#simpleCondition.
    def visitSimpleCondition(self, ctx:Cobol85Parser.SimpleConditionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#classCondition.
    def visitClassCondition(self, ctx:Cobol85Parser.ClassConditionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#conditionNameReference.
    def visitConditionNameReference(self, ctx:Cobol85Parser.ConditionNameReferenceContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#conditionNameSubscriptReference.
    def visitConditionNameSubscriptReference(self, ctx:Cobol85Parser.ConditionNameSubscriptReferenceContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#relationCondition.
    def visitRelationCondition(self, ctx:Cobol85Parser.RelationConditionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#relationSignCondition.
    def visitRelationSignCondition(self, ctx:Cobol85Parser.RelationSignConditionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#relationArithmeticComparison.
    def visitRelationArithmeticComparison(self, ctx:Cobol85Parser.RelationArithmeticComparisonContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#relationCombinedComparison.
    def visitRelationCombinedComparison(self, ctx:Cobol85Parser.RelationCombinedComparisonContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#relationCombinedCondition.
    def visitRelationCombinedCondition(self, ctx:Cobol85Parser.RelationCombinedConditionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#relationalOperator.
    def visitRelationalOperator(self, ctx:Cobol85Parser.RelationalOperatorContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#abbreviation.
    def visitAbbreviation(self, ctx:Cobol85Parser.AbbreviationContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#identifier.
    def visitIdentifier(self, ctx:Cobol85Parser.IdentifierContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#tableCall.
    def visitTableCall(self, ctx:Cobol85Parser.TableCallContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#functionCall.
    def visitFunctionCall(self, ctx:Cobol85Parser.FunctionCallContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#referenceModifier.
    def visitReferenceModifier(self, ctx:Cobol85Parser.ReferenceModifierContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#characterPosition.
    def visitCharacterPosition(self, ctx:Cobol85Parser.CharacterPositionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#length.
    def visitLength(self, ctx:Cobol85Parser.LengthContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#subscript_.
    def visitSubscript_(self, ctx:Cobol85Parser.Subscript_Context):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#argument.
    def visitArgument(self, ctx:Cobol85Parser.ArgumentContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#qualifiedDataName.
    def visitQualifiedDataName(self, ctx:Cobol85Parser.QualifiedDataNameContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#qualifiedDataNameFormat1.
    def visitQualifiedDataNameFormat1(self, ctx:Cobol85Parser.QualifiedDataNameFormat1Context):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#qualifiedDataNameFormat2.
    def visitQualifiedDataNameFormat2(self, ctx:Cobol85Parser.QualifiedDataNameFormat2Context):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#qualifiedDataNameFormat3.
    def visitQualifiedDataNameFormat3(self, ctx:Cobol85Parser.QualifiedDataNameFormat3Context):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#qualifiedDataNameFormat4.
    def visitQualifiedDataNameFormat4(self, ctx:Cobol85Parser.QualifiedDataNameFormat4Context):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#qualifiedInData.
    def visitQualifiedInData(self, ctx:Cobol85Parser.QualifiedInDataContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#inData.
    def visitInData(self, ctx:Cobol85Parser.InDataContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#inFile.
    def visitInFile(self, ctx:Cobol85Parser.InFileContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#inMnemonic.
    def visitInMnemonic(self, ctx:Cobol85Parser.InMnemonicContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#inSection.
    def visitInSection(self, ctx:Cobol85Parser.InSectionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#inLibrary.
    def visitInLibrary(self, ctx:Cobol85Parser.InLibraryContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#inTable.
    def visitInTable(self, ctx:Cobol85Parser.InTableContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#alphabetName.
    def visitAlphabetName(self, ctx:Cobol85Parser.AlphabetNameContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#assignmentName.
    def visitAssignmentName(self, ctx:Cobol85Parser.AssignmentNameContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#basisName.
    def visitBasisName(self, ctx:Cobol85Parser.BasisNameContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#cdName.
    def visitCdName(self, ctx:Cobol85Parser.CdNameContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#className.
    def visitClassName(self, ctx:Cobol85Parser.ClassNameContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#computerName.
    def visitComputerName(self, ctx:Cobol85Parser.ComputerNameContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#conditionName.
    def visitConditionName(self, ctx:Cobol85Parser.ConditionNameContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#dataName.
    def visitDataName(self, ctx:Cobol85Parser.DataNameContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#dataDescName.
    def visitDataDescName(self, ctx:Cobol85Parser.DataDescNameContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#environmentName.
    def visitEnvironmentName(self, ctx:Cobol85Parser.EnvironmentNameContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#fileName.
    def visitFileName(self, ctx:Cobol85Parser.FileNameContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#functionName.
    def visitFunctionName(self, ctx:Cobol85Parser.FunctionNameContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#indexName.
    def visitIndexName(self, ctx:Cobol85Parser.IndexNameContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#languageName.
    def visitLanguageName(self, ctx:Cobol85Parser.LanguageNameContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#libraryName.
    def visitLibraryName(self, ctx:Cobol85Parser.LibraryNameContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#localName.
    def visitLocalName(self, ctx:Cobol85Parser.LocalNameContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#mnemonicName.
    def visitMnemonicName(self, ctx:Cobol85Parser.MnemonicNameContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#paragraphName.
    def visitParagraphName(self, ctx:Cobol85Parser.ParagraphNameContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#procedureName.
    def visitProcedureName(self, ctx:Cobol85Parser.ProcedureNameContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#programName.
    def visitProgramName(self, ctx:Cobol85Parser.ProgramNameContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#recordName.
    def visitRecordName(self, ctx:Cobol85Parser.RecordNameContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#reportName.
    def visitReportName(self, ctx:Cobol85Parser.ReportNameContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#routineName.
    def visitRoutineName(self, ctx:Cobol85Parser.RoutineNameContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#screenName.
    def visitScreenName(self, ctx:Cobol85Parser.ScreenNameContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#sectionName.
    def visitSectionName(self, ctx:Cobol85Parser.SectionNameContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#systemName.
    def visitSystemName(self, ctx:Cobol85Parser.SystemNameContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#symbolicCharacter.
    def visitSymbolicCharacter(self, ctx:Cobol85Parser.SymbolicCharacterContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#textName.
    def visitTextName(self, ctx:Cobol85Parser.TextNameContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#cobolWord.
    def visitCobolWord(self, ctx:Cobol85Parser.CobolWordContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#literal.
    def visitLiteral(self, ctx:Cobol85Parser.LiteralContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#booleanLiteral.
    def visitBooleanLiteral(self, ctx:Cobol85Parser.BooleanLiteralContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#numericLiteral.
    def visitNumericLiteral(self, ctx:Cobol85Parser.NumericLiteralContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#integerLiteral.
    def visitIntegerLiteral(self, ctx:Cobol85Parser.IntegerLiteralContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#cicsDfhRespLiteral.
    def visitCicsDfhRespLiteral(self, ctx:Cobol85Parser.CicsDfhRespLiteralContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#cicsDfhValueLiteral.
    def visitCicsDfhValueLiteral(self, ctx:Cobol85Parser.CicsDfhValueLiteralContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#figurativeConstant.
    def visitFigurativeConstant(self, ctx:Cobol85Parser.FigurativeConstantContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#specialRegister.
    def visitSpecialRegister(self, ctx:Cobol85Parser.SpecialRegisterContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85Parser#commentEntry.
    def visitCommentEntry(self, ctx:Cobol85Parser.CommentEntryContext):
        return self.visitChildren(ctx)



del Cobol85Parser