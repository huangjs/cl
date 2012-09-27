<?xml version="1.0"?>
<!--
    symbols.xsl

    Copyright (c) 2007, Jack D. Unrue
-->
<xsl:stylesheet
  xmlns:exsl="http://exslt.org/common"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  version="1.0">

  <xsl:output method="xml" omit-xml-declaration="yes" indent="yes"/>

  <xsl:include href="utils.xsl"/>

  <xsl:variable name="win32-table" select="document('win32-api-table.xml')"/>
  <xsl:variable name="clhs-table"  select="document('clhs-table.xml')"/>

  <xsl:template name="emit-index-term">
    <indexterm><primary><xsl:value-of select="@name"/></primary></indexterm>
  </xsl:template>

  <xsl:template name="emit-page-type">
    <xsl:param name="page-type"/>

      <xsl:element name="para">
        <xsl:attribute name="role">normal</xsl:attribute>
        [<xsl:call-template name="upcase">
           <xsl:with-param name="orig-text"><xsl:value-of select="../@name"/></xsl:with-param>
         </xsl:call-template>]
        <xsl:value-of select="$page-type"/>
      </xsl:element>
  </xsl:template>

  <xsl:template match="para | emphasis | itemizedlist | listitem | ulink | code | programlisting">
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:apply-templates/>
    </xsl:copy>
  </xsl:template>

  <xsl:template name="emit-sorted-table">
    <xsl:param name="col2-width"/>

    <xsl:element name="informaltable">
      <xsl:attribute name="frame">none</xsl:attribute>
      <xsl:element name="tgroup">
        <xsl:attribute name="cols">2</xsl:attribute>
        <xsl:element name="colspec">
          <xsl:attribute name="colwidth">*</xsl:attribute>
        </xsl:element>
        <xsl:element name="colspec">
          <xsl:attribute name="colwidth"><xsl:value-of select="$col2-width"/></xsl:attribute>
        </xsl:element>
        <xsl:element name="tbody">
          <xsl:apply-templates>
            <xsl:sort select="@name" order="ascending" case-order="upper-first"/>
          </xsl:apply-templates>
        </xsl:element>
      </xsl:element>
    </xsl:element>
  </xsl:template>

  <xsl:template name="emit-unsorted-table">
    <xsl:param name="col2-width"/>

    <xsl:element name="informaltable">
      <xsl:attribute name="frame">none</xsl:attribute>
      <xsl:element name="tgroup">
        <xsl:attribute name="cols">2</xsl:attribute>
        <xsl:element name="colspec">
          <xsl:attribute name="colwidth">*</xsl:attribute>
        </xsl:element>
        <xsl:element name="colspec">
          <xsl:attribute name="colwidth"><xsl:value-of select="$col2-width"/></xsl:attribute>
        </xsl:element>
        <xsl:element name="tbody">
          <xsl:apply-templates/>
        </xsl:element>
      </xsl:element>
    </xsl:element>
  </xsl:template>

  <xsl:template name="find-arg">
    <xsl:param name="index"/>
    <xsl:variable name="raw-text">
      <xsl:choose>
        <xsl:when test="../syntax/arguments">
          <xsl:value-of select="(../syntax/arguments/argument)[position()=$index]/@name"/>
        </xsl:when>
        <xsl:when test="../../syntax/arguments">
          <xsl:value-of select="(../../syntax/arguments/argument)[position()=$index]/@name"/>
        </xsl:when>
        <xsl:when test="../../../syntax/arguments">
          <xsl:value-of select="(../../../syntax/arguments/argument)[position()=$index]/@name"/>
        </xsl:when>
        <xsl:when test="../../../../syntax/arguments">
          <xsl:value-of select="(../../../../syntax/arguments/argument)[position()=$index]/@name"/>
        </xsl:when>
        <xsl:when test="../../../../../syntax/arguments">
          <xsl:value-of select="(../../../../../syntax/arguments/argument)[position()=$index]/@name"/>
        </xsl:when>
        <xsl:when test="../../../../../../syntax/arguments">
          <xsl:value-of select="(../../../../../../syntax/arguments/argument)[position()=$index]/@name"/>
        </xsl:when>
        <xsl:when test="../../../../../../../syntax/arguments">
          <xsl:value-of select="(../../../../../../../syntax/arguments/argument)[position()=$index]/@name"/>
        </xsl:when>
        <xsl:when test="../../../../../../../../syntax/arguments">
          <xsl:value-of select="(../../../../../../../../syntax/arguments/argument)[position()=$index]/@name"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:message terminate="yes">
            <xsl:text>symbols.xsl: could not find argument </xsl:text><xsl:value-of select="$index - 1"/>
          </xsl:message>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <emphasis>
      <xsl:call-template name="first-word">
        <xsl:with-param name="raw-text"><xsl:value-of select="$raw-text"/></xsl:with-param>
      </xsl:call-template>
    </emphasis>
  </xsl:template>

  <xsl:template match="arg0">
    <xsl:call-template name="find-arg">
      <xsl:with-param name="index">1</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="arg1">
    <xsl:call-template name="find-arg">
      <xsl:with-param name="index">2</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="arg2">
    <xsl:call-template name="find-arg">
      <xsl:with-param name="index">3</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="arg3">
    <xsl:call-template name="find-arg">
      <xsl:with-param name="index">4</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="arg4">
    <xsl:call-template name="find-arg">
      <xsl:with-param name="index">5</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="arg5">
    <xsl:call-template name="find-arg">
      <xsl:with-param name="index">6</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="arg6">
    <xsl:call-template name="find-arg">
      <xsl:with-param name="index">7</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="reftopic">
    <xsl:element name="link">
      <xsl:attribute name="linkend"><xsl:value-of select="."/></xsl:attribute>
      <xsl:choose>
        <xsl:when test="@label">
          <xsl:value-of select="@label"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="."/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:element>
  </xsl:template>

  <xsl:template match="refclhs">
    <xsl:variable name="tmp" select="string()"/>
    <xsl:element name="ulink">
      <xsl:attribute name="url">
        <xsl:value-of select="$clhs-table//entry[string(@name)=$tmp]/@url"/>
      </xsl:attribute>
      <xsl:value-of select="."/>
    </xsl:element>
  </xsl:template>

  <xsl:template match="refwin32api">
    <xsl:variable name="tmp" select="string()"/>
    <xsl:element name="ulink">
      <xsl:attribute name="url">
        <xsl:value-of select="$win32-table//entry[string(@name)=$tmp]/@url"/>
      </xsl:attribute>
      <xsl:value-of select="."/>
    </xsl:element>
  </xsl:template>

  <xsl:template match="syntax">
    <xsl:element name="bridgehead">
      <xsl:attribute name="renderas">sect2</xsl:attribute>
      syntax
    </xsl:element>

    <para role="normal">
      <xsl:attribute name="role">normal</xsl:attribute>
      (<xsl:value-of select="concat(../../@name,':',../@name)"/>
       <xsl:element name="emphasis">
         <xsl:for-each select="arguments/argument | arguments/notarg">
           <xsl:value-of select="concat(' ', @name)"/>
         </xsl:for-each>
       </xsl:element>) =&gt;
      <xsl:element name="emphasis">
        <xsl:for-each select="return/*">
          <xsl:apply-templates select="."/>
          <xsl:if test="not(position()=last())">, </xsl:if>
        </xsl:for-each>
      </xsl:element>
    </para>

    <xsl:if test="@with-setf">
      <xsl:element name="para">
        <xsl:attribute name="role">normal</xsl:attribute>
        (setf (<xsl:value-of select="concat(../../@name,':',../@name,' ')"/>
        <emphasis>
          <xsl:for-each select="arguments/argument | arguments/notarg">
            <xsl:value-of select="concat(' ', @name)"/>
          </xsl:for-each>
        </emphasis>
        <xsl:element name="emphasis">
          <xsl:for-each select="return/*">
            <xsl:if test="position()=last()">)</xsl:if>
            <xsl:value-of select="' '"/>
            <xsl:apply-templates select="."/>
          </xsl:for-each>
        </xsl:element>)
      </xsl:element>
    </xsl:if>

    <xsl:apply-templates select="para"/>

    <xsl:apply-templates select="arguments"/>
  </xsl:template>

  <xsl:template match="argument">
    <xsl:element name="row">
      <xsl:element name="entry">
        <xsl:attribute name="valign">top</xsl:attribute>
        <xsl:element name="para">
          <xsl:attribute name="role">normal</xsl:attribute>
          <xsl:element name="emphasis">
            <xsl:call-template name="first-word">
              <xsl:with-param name="raw-text"><xsl:value-of select="@name"/></xsl:with-param>
            </xsl:call-template>
          </xsl:element>
        </xsl:element>
      </xsl:element>
      <xsl:element name="entry">
        <xsl:attribute name="valign">top</xsl:attribute>
        <xsl:element name="para">
          <xsl:attribute name="role">normal</xsl:attribute>
          <xsl:apply-templates select="description"/>
        </xsl:element>
      </xsl:element>
    </xsl:element>
  </xsl:template>

  <xsl:template match="arguments">
    <xsl:element name="bridgehead">
      <xsl:attribute name="renderas">sect2</xsl:attribute>
      arguments
    </xsl:element>
    <xsl:call-template name="emit-unsorted-table">
      <xsl:with-param name="col2-width">5*</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="initargs">
    <xsl:element name="bridgehead">
      <xsl:attribute name="renderas">sect2</xsl:attribute>
      initargs
    </xsl:element>
    <xsl:for-each select="argument">
      <xsl:element name="indexterm">
        <xsl:element name="primary">
          <xsl:value-of select="@name"/>
        </xsl:element>
      </xsl:element>
    </xsl:for-each>
    <xsl:call-template name="emit-sorted-table">
      <xsl:with-param name="col2-width">5*</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="enum">
    <xsl:for-each select="argument">
      <xsl:element name="indexterm">
        <xsl:element name="primary">
          <xsl:value-of select="@name"/>
        </xsl:element>
      </xsl:element>
    </xsl:for-each>
    <xsl:call-template name="emit-sorted-table">
      <xsl:with-param name="col2-width">5*</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="inherits">
    <xsl:element name="row">
      <xsl:element name="entry">
        <xsl:attribute name="valign">top</xsl:attribute>
        <xsl:element name="para">
          <xsl:attribute name="role">normal</xsl:attribute>
          inherits:
        </xsl:element>
      </xsl:element>
      <xsl:element name="entry">
        <xsl:attribute name="valign">top</xsl:attribute>
        <para role="normal">
          <xsl:for-each select="*">
            <xsl:sort select="substring-after(.,':')" order="ascending" case-order="upper-first"/>
            <xsl:apply-templates select="."/>
            <xsl:if test="not(position()=last())">, </xsl:if>
          </xsl:for-each>
        </para>
      </xsl:element>
    </xsl:element>
  </xsl:template>

  <xsl:template match="inheritedby">
    <xsl:element name="row">
      <xsl:element name="entry">
        <xsl:attribute name="valign">top</xsl:attribute>
        <xsl:element name="para">
          <xsl:attribute name="role">normal</xsl:attribute>
          inherited by:
        </xsl:element>
      </xsl:element>
      <xsl:element name="entry">
        <xsl:attribute name="valign">top</xsl:attribute>
        <para role="normal">
        <xsl:for-each select="*">
          <xsl:sort select="substring-after(.,':')" order="ascending" case-order="upper-first"/>
          <xsl:apply-templates select="."/>
          <xsl:if test="not(position()=last())">, </xsl:if>
        </xsl:for-each>
        </para>
      </xsl:element>
    </xsl:element>
  </xsl:template>

  <xsl:template match="hierarchy">
    <xsl:call-template name="emit-sorted-table">
      <xsl:with-param name="col2-width">8*</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="*/seealso">
    <xsl:element name="bridgehead">
      <xsl:attribute name="renderas">sect2</xsl:attribute>
      see also
    </xsl:element>

    <xsl:element name="para">
      <xsl:attribute name="role">normal</xsl:attribute>

      <xsl:for-each select="*">
        <xsl:sort select="substring-after(.,':')" order="ascending" case-order="upper-first"/>
        <xsl:apply-templates select="."/>
        <xsl:if test="not(position()=last())">, </xsl:if>
      </xsl:for-each>
    </xsl:element>
  </xsl:template>

  <xsl:template match="/symbols/*/description">
    <xsl:element name="bridgehead">
      <xsl:attribute name="renderas">sect2</xsl:attribute>
      description
    </xsl:element>

    <xsl:element name="para">
      <xsl:attribute name="role">normal</xsl:attribute>
      <xsl:apply-templates/>
    </xsl:element>
  </xsl:template>

  <xsl:template name="emit-type-section">
    <xsl:param name="page-type"/>

    <xsl:element name="section">
      <xsl:attribute name="id"><xsl:call-template name="create-id"/></xsl:attribute>

      <xsl:call-template name="emit-index-term"/>

      <xsl:element name="title"><xsl:value-of select="@name"/></xsl:element>

      <xsl:call-template name="emit-page-type">
        <xsl:with-param name="page-type">[<xsl:value-of select="$page-type"/>]</xsl:with-param>
      </xsl:call-template>

      <xsl:apply-templates/>
    </xsl:element>

  </xsl:template>

  <xsl:template name="emit-function-section">
    <xsl:param name="page-type"/>

    <xsl:element name="section">
      <xsl:attribute name="id"><xsl:call-template name="create-id"/></xsl:attribute>

      <xsl:call-template name="emit-index-term"/>

      <xsl:element name="title"><xsl:value-of select="@name"/></xsl:element>

      <xsl:call-template name="emit-page-type">
        <xsl:with-param name="page-type">[<xsl:value-of select="$page-type"/>]</xsl:with-param>
      </xsl:call-template>

      <xsl:apply-templates/>
    </xsl:element>
  </xsl:template>

  <xsl:template match="class">
    <xsl:call-template name="emit-type-section">
      <xsl:with-param name="page-type">Class</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="structure">
    <xsl:call-template name="emit-type-section">
      <xsl:with-param name="page-type">Structure</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="condition">
    <xsl:call-template name="emit-type-section">
      <xsl:with-param name="page-type">Condition</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="function">
    <xsl:call-template name="emit-function-section">
      <xsl:with-param name="page-type">Function</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="generic-function">
    <xsl:call-template name="emit-function-section">
      <xsl:with-param name="page-type">Generic Function</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="slot-accessor">
    <xsl:call-template name="emit-function-section">
      <xsl:with-param name="page-type">Slot Accessor</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="slot-reader">
    <xsl:call-template name="emit-function-section">
      <xsl:with-param name="page-type">Slot Reader</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="macro">
    <xsl:call-template name="emit-function-section">
      <xsl:with-param name="page-type">Macro</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="/symbols">
    <data>
      <xsl:apply-templates/>
    </data>
  </xsl:template>

</xsl:stylesheet>
